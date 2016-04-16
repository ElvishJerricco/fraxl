{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Fraxl where

import           Control.Applicative.Free
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Free.Ap hiding (Pure)
import           Data.Dependent.Map          (DMap)
import qualified Data.Dependent.Map          as DMap
import           Data.Dependent.OpenUnion

type FreerT f = FreeT (Ap f)
type Fraxl r = FreerT (Union r)
type Fetch f m a = f a -> m (m a)

class Monad m => DataSource f m where
  fetch :: Fetch f m a

instance Monad m => DataSource (Union '[]) m where
  fetch _ = error "Not possible - empty union"

instance ( DataSource f m
         , DataSource (Union r) m)
         => DataSource (Union (f ': r)) m where
  fetch u = case prj u of
    Right (fx :: f x) -> fetch fx
    Left u' -> fetch u'

runFraxl :: forall f m a. DataSource f m => FreerT f m a -> m a
runFraxl = iterT (run join) where
  run :: (m a' -> m x) -> Ap f a' -> m x
  run k (Pure a) = k (return a)
  run k (Ap fa ff) = flip run ff $ \f' -> do
    a' <- fetch fa
    k (f' <*> a')

simpleAsyncFetch :: MonadIO m => (f a -> IO a) -> Fetch f m a
simpleAsyncFetch fetchIO fa
  = liftIO . wait <$> liftIO (async $ fetchIO fa)

newtype CachedFetch f a = CachedFetch (f a)

instance ( DataSource f m
         , MonadTrans t
         , MonadState (DMap f MVar) (t m)
         , DMap.GCompare f
         , MonadIO (t m))
         => DataSource (CachedFetch f) (t m) where
  fetch (CachedFetch fa) = do
    cache <- get
    case DMap.lookup fa cache of
      Just mvar -> return $ liftIO (readMVar mvar)
      Nothing -> do
        mvar <- liftIO newEmptyMVar
        put (DMap.insert fa mvar cache)
        fmap
          (>>= \a -> liftIO (putMVar mvar a) >> return a)
          (lift $ lift <$> fetch fa)

runCachedFraxl :: forall m f a.
                  ( MonadIO m
                  , DataSource f m
                  , DMap.GCompare f)
               => FreerT f m a -> DMap f MVar -> m (a, DMap f MVar)
runCachedFraxl a cache = let
  statefulA :: FreerT f (StateT (DMap f MVar) m) a
  statefulA = hoistFreeT lift a
  cachedA :: FreerT (CachedFetch f) (StateT (DMap f MVar) m) a
  cachedA = transFreeT (hoistAp CachedFetch) statefulA
  in runStateT (runFraxl cachedA) cache

evalCachedFraxl :: forall m f a.
                   ( MonadIO m
                   , DataSource f m
                   , DMap.GCompare f)
                => FreerT f m a -> m a
evalCachedFraxl a = fst <$> runCachedFraxl a DMap.empty
