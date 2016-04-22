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
  , DataSource(..)
  , runFraxl
  , simpleAsyncFetch
  -- * The Sequence of Effects
  , ASeq(..)
  , reduceASeq
  , hoistASeq
  , traverseASeq
  , rebaseASeq
  -- * Caching
  , CachedFetch(..)
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
import           Control.Monad.Trans.Free.Ap
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DMap
import           Data.Dependent.OpenUnion
import           Data.GADT.Compare

-- | Fraxl is based on a particular Freer monad.
-- This Freer monad has applicative optimization,
-- which is used to parallelize effects.
type FreerT f = FreeT (Ap f)

-- | Fraxl is just the 'FreerT' monad transformer, applied with 'Union'.
-- This is because 'Fraxl' is just a free monad over a variety of data sources.
type Fraxl r = FreerT (Union r)

-- | Data sources produce 'Fetch' functions.
-- They take a sequence of effeects as an argument,
-- and return a corresponding sequence of monadic actions
-- which are used to wait on the results.
type Fetch f m a = ASeq f a -> m (ASeq m a)

-- | A data source is an effect @f@ that operates in some monad @m@.
-- Given a sequence of effects,
-- a data source should use @m@ to prepare a corresponding sequence of results.
class Monad m => DataSource f m where
  fetch :: Fetch f m q

instance Monad m => DataSource (Union '[]) m where
  fetch ANil = return ANil
  fetch (ACons _ _) = error "Not possible - empty union"

instance ( DataSource f m
         , DataSource (Union r) m)
         => DataSource (Union (f ': r)) m where
  fetch list = (\(_, _, x) -> x) <$> runUnion ANil ANil list where
    runUnion :: ASeq f a
        -> ASeq (Union r) a'
        -> ASeq (Union (f ': r)) a''
        -> m (ASeq m a, ASeq m a', ASeq m a'')
    runUnion flist ulist ANil = (, , ANil) <$> fetch flist <*> fetch ulist
    runUnion flist ulist (ACons u us) = case prj u of
      Right (fa :: f a) -> fmap
        (\(ACons ma ms, other, rest) -> (ms, other, ACons ma rest))
        (runUnion (ACons fa flist) ulist us)
      Left u' -> fmap
        (\(other, ACons ma ms, rest) -> (other, ms, ACons ma rest))
        (runUnion flist (ACons u' ulist) us)

-- | Runs a Fraxl computation.
-- This takes 'FreerT' as a parameter rather than 'Fraxl',
-- because 'Fraxl' is meant for a union of effects,
-- but it should be possible to run a singleton effect.
runFraxl :: forall f m a. DataSource f m => FreerT f m a -> m a
runFraxl = iterT $ \a -> unAp a
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

instance ( DataSource f m
         , MonadTrans t
         , MonadState (DMap f MVar) (t m)
         , GCompare f
         , MonadIO (t m))
         => DataSource (CachedFetch f) (t m) where
  fetch list = snd <$> runCached ANil list where
    runCached :: ASeq f a
              -> ASeq (CachedFetch f) a'
              -> t m (ASeq (t m) a, ASeq (t m) a')
    runCached flist ANil = (, ANil) <$> lift (hoistASeq lift <$> fetch flist)
    runCached flist (ACons (CachedFetch f) fs) = do
      cache <- get
      case DMap.lookup f cache of
        Just mvar -> fmap
          (second (ACons (liftIO $ readMVar mvar)))
          (runCached flist fs)
        Nothing -> do
          (mvar :: MVar x) <- liftIO newEmptyMVar
          put (DMap.insert f mvar cache)
          let store :: t m x -> t m x
              store m = m >>= \a -> liftIO (putMVar mvar a) >> return a
          fmap
            (\(ACons m ms, rest) -> (ms, ACons (store m) rest))
            (runCached (ACons f flist) fs)

-- | Runs a Fraxl computation with caching using a given starting cache.
-- Alongside the result, it returns the final cache.
runCachedFraxl :: forall m f a.
                  ( MonadIO m
                  , DataSource f m
                  , GCompare f)
                  => FreerT f m a -> DMap f MVar -> m (a, DMap f MVar)
runCachedFraxl a cache = let
  statefulA :: FreerT f (StateT (DMap f MVar) m) a
  statefulA = hoistFreeT lift a
  cachedA :: FreerT (CachedFetch f) (StateT (DMap f MVar) m) a
  cachedA = transFreeT (hoistAp CachedFetch) statefulA
  in runStateT (runFraxl cachedA) cache

-- | Like 'runCachedFraxl', except it starts with an empty cache
-- and discards the final cache.
evalCachedFraxl :: forall m f a.
                   ( MonadIO m
                   , DataSource f m
                   , GCompare f)
                   => FreerT f m a -> m a
evalCachedFraxl a = fst <$> runCachedFraxl a DMap.empty
