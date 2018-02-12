module Control.Monad.Trans.Fraxl
  (
  -- * The Fraxl Monad
    FreerT
  , Fraxl
  , Fetch
  , runFraxl
  , simpleAsyncFetch
  , fetchNil
  , (|:|)
  , hoistFetch
  , transFetch
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
  -- * Union
  , Union(..)
  ) where

import           Control.Applicative.Fraxl.Free
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Fraxl.Free
import           Data.Dependent.Map             (DMap)
import qualified Data.Dependent.Map             as DMap
import           Data.GADT.Compare
import           Data.Union

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
fetchNil :: Applicative m => Fetch (Union '[]) m a
fetchNil ANil = pure ANil
fetchNil _ = error "Not possible - empty union"

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
  runUnion flist ulist (ACons u us) = case u of
    UnionLeft fa -> fmap
      (\(ACons ma ms, other, rest) -> (ms, other, ACons ma rest))
      (runUnion (ACons fa flist) ulist us)
    UnionRight u' -> fmap
      (\(other, ACons ma ms, rest) -> (other, ms, ACons ma rest))
      (runUnion flist (ACons u' ulist) us)

infixr 5 |:|

-- | Hoist a 'Fetch' function into a different result monad.
hoistFetch :: Functor m => (forall x. m x -> n x) -> Fetch f m a -> Fetch f n a
hoistFetch u f = u . fmap (hoistASeq u) . f

-- | Translate a 'Fetch' function from @f@ requests, to @g@ requests.
transFetch :: (forall x. g x -> f x) -> Fetch f m a -> Fetch g m a
transFetch u f list = f (hoistASeq u list)

-- | Runs a Fraxl computation, using a given 'Fetch' function for @f@.
-- This takes 'FreerT' as a parameter rather than 'Fraxl',
-- because 'Fraxl' is meant for a union of effects,
-- but it should be possible to run a singleton effect.
runFraxl :: Monad m => (forall a'. Fetch f m a') -> FreerT f m a -> m a
runFraxl fetch = iterT $ \a -> unAp a
  (\f s -> join (reduceASeq <$> fetch s) >>= f) (const id) ANil

-- | A simple method of turning an 'IO' bound computation
-- into a concurrent 'Fetch'.
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
  cachedA :: FreerT (CachedFetch f) (StateT (DMap f MVar) m) a
  cachedA = transFreeT (hoistAp CachedFetch) (hoistFreeT lift a)
  in runStateT (runFraxl (fetchCached fetch) cachedA) cache

-- | Like 'runCachedFraxl', except it starts with an empty cache
-- and discards the final cache.
evalCachedFraxl :: forall m f a.
                   ( MonadIO m
                   , GCompare f)
                   => (forall a'. Fetch f m a') -> FreerT f m a -> m a
evalCachedFraxl fetch a = fst <$> runCachedFraxl fetch a DMap.empty
