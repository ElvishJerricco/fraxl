{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Trans.Fraxl
  (
  -- * The Fraxl Monad
    FreerT
  , Fraxl
  , Fetch
  , runFraxl
  , simpleAsyncFetch
  , fNil
  , (|:|)
  -- * The Sequence of Effects
  , ASeq(..)
  , reduceASeq
  , hoistASeq
  , traverseASeq
  , rebaseASeq
  -- * Caching
  , CachedFetch(..)
  , fetchCached
  , runCachedFraxl
  , evalCachedFraxl
  , module Data.GADT.Compare
  ) where

import           Control.Applicative.Free.Fast
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Fraxl.Free
import           Data.Dependent.Map             (DMap)
import qualified Data.Dependent.Map             as DMap
import           Data.Dependent.OpenUnion
import           Data.GADT.Compare

-- | Fraxl is based on a particular Freer monad.
-- This Freer monad has applicative optimization,
-- which is used to parallelize effects.
type FreerT f = FreeT (Ap f)

-- | Fraxl is just the 'FreerT' monad transformer, applied with 'Union'.
-- This is because 'Fraxl' is just a free monad over a variety of data sources.
type Fraxl r = FreerT (Union r)

-- | A data source is an effect @f@ that operates in some monad @m@.
-- Given a sequence of effects,
-- a data source should use @m@ to prepare a corresponding sequence of results.
type Fetch f m a = ASeq f a -> m (ASeq m a)

-- | Fetch empty union.
-- Only necessary to terminate a list of 'Fetch' functions for @Fetch (Union r)@
fNil :: Applicative m => Fetch (Union '[]) m a
fNil ANil = pure ANil
fNil _ = error "Not possible - empty union"

-- | Like '(:)' for constructing @Fetch (Union (f ': r))@
(|:|) :: forall f r a m. Monad m
       => (forall a'. Fetch f m a')
       -> (forall a'. Fetch (Union r) m a')
       -> Fetch (Union (f ': r)) m a
(fetch |:| fetchU) list = (\(_, _, x) -> x) <$> runUnion ANil ANil list where
  runUnion :: ASeq f x
           -> ASeq (Union r) y
           -> ASeq (Union (f ': r)) z
           -> m (ASeq m x, ASeq m y, ASeq m z)
  runUnion flist ulist ANil = (, , ANil) <$> fetch flist <*> fetchU ulist
  runUnion flist ulist (ACons u us) = case prj u of
    Right (fa :: f x) -> fmap
      (\(ACons ma ms, other, rest) -> (ms, other, ACons ma rest))
      (runUnion (ACons fa flist) ulist us)
    Left u' -> fmap
      (\(other, ACons ma ms, rest) -> (other, ms, ACons ma rest))
      (runUnion flist (ACons u' ulist) us)

infixr 5 |:|

-- | Runs a Fraxl computation, using a given 'Fetch' function for @f@.
-- This takes 'FreerT' as a parameter rather than 'Fraxl',
-- because 'Fraxl' is meant for a union of effects,
-- but it should be possible to run a singleton effect.
runFraxl :: Monad m => (forall a'. Fetch f m a') -> FreerT f m a -> m a
runFraxl fetch = iterT $ \a -> unAp a
  (\f s -> join (reduceASeq <$> fetch s) >>= f) (const id) ANil

-- | A simple method of turning an 'IO' bound computation
-- into a 'DataSource' 'Fetch'.
simpleAsyncFetch :: MonadIO m
                    => (forall x. f x -> IO x)
                    -> Fetch f m a
simpleAsyncFetch fetchIO
  = traverseASeq (fmap (liftIO . wait) . liftIO . async . fetchIO)

-- | Caching in Fraxl works by translating @FreerT f@ into
-- @FreerT (CachedFetch f)@, then running with 'CachedFetch''s DataSource.
-- That instance requires 'f' to to have a 'GCompare' instance.
--
-- The 'CachedFetch' instance uses a 'MonadState' to track cached requests.
-- The state variable is a 'DMap' from the 'dependent-map' package.
-- Keys are requests, and values are 'MVar's of the results.
newtype CachedFetch f a = CachedFetch (f a)

fetchCached :: forall t m f a.
            ( Monad m
            , MonadTrans t
            , MonadState (DMap f MVar) (t m)
            , GCompare f
            , MonadIO (t m))
            => (forall a'. Fetch f m a') -> Fetch (CachedFetch f) (t m) a
fetchCached fetch list = snd <$> runCached ANil list where
  runCached :: ASeq f x
            -> ASeq (CachedFetch f) y
            -> t m (ASeq (t m) x, ASeq (t m) y)
  runCached flist ANil = (, ANil) <$> lift (hoistASeq lift <$> fetch flist)
  runCached flist (ACons (CachedFetch f) fs) = do
    cache <- get
    case DMap.lookup f cache of
      Just mvar -> fmap
        (second (ACons (liftIO $ readMVar mvar)))
        (runCached flist fs)
      Nothing -> do
        (mvar :: MVar z) <- liftIO newEmptyMVar
        put (DMap.insert f mvar cache)
        let store :: t m z -> t m z
            store m = m >>= \a -> liftIO (putMVar mvar a) >> return a
        fmap
          (\(ACons m ms, rest) -> (ms, ACons (store m) rest))
          (runCached (ACons f flist) fs)

-- | Runs a Fraxl computation with caching using a given starting cache.
-- Alongside the result, it returns the final cache.
runCachedFraxl :: forall m f a.
                  ( MonadIO m
                  , GCompare f)
                  => (forall a'. Fetch f m a')
                  -> FreerT f m a
                  -> DMap f MVar
                  -> m (a, DMap f MVar)
runCachedFraxl fetch a cache = let
  statefulA :: FreerT f (StateT (DMap f MVar) m) a
  statefulA = hoistFreeT lift a
  cachedA :: FreerT (CachedFetch f) (StateT (DMap f MVar) m) a
  cachedA = transFreeT (hoistAp CachedFetch) statefulA
  in runStateT (runFraxl (fetchCached fetch) cachedA) cache

-- | Like 'runCachedFraxl', except it starts with an empty cache
-- and discards the final cache.
evalCachedFraxl :: forall m f a.
                   ( MonadIO m
                   , GCompare f)
                   => (forall a'. Fetch f m a') -> FreerT f m a -> m a
evalCachedFraxl fetch a = fst <$> runCachedFraxl fetch a DMap.empty
