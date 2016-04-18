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

module Control.Fraxl where

import           Control.Applicative.Free
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Monad.Trans.Free.Ap hiding (Pure)
import           Data.Dependent.Map          (DMap)
import qualified Data.Dependent.Map          as DMap
import           Data.Dependent.OpenUnion
import           Data.Vinyl

type FreerT f = FreeT (Ap f)
type Fraxl r = FreerT (Union r)
type Fetch f m q = Rec f q -> m (Rec m q)

class Monad m => DataSource f m where
  fetch :: Fetch f m q

instance Monad m => DataSource (Union '[]) m where
  fetch RNil = return RNil
  fetch (_ :& _) = error "Not possible - empty union"

instance ( DataSource f m
         , DataSource (Union r) m)
         => DataSource (Union (f ': r)) m where
  fetch list = runContT (run RNil RNil list) (\(_, _, x) -> return x) where
    run :: Rec f q
        -> Rec (Union r) q'
        -> Rec (Union (f ': r)) q''
        -> ContT x m (Rec m q, Rec m q', Rec m q'')
    run flist ulist RNil = lift $ (, , RNil) <$> fetch flist <*> fetch ulist
    run flist ulist (u :& us) = case prj u of
      Right (fa :: f a) -> fmap
        (\(ma :& ms, other, rest) -> (ms, other, ma :& rest))
        (run (fa :& flist) ulist us)
      Left u' -> fmap
        (\(other, ma :& ms, rest) -> (other, ms, ma :& rest))
        (run flist (u' :& ulist) us)

runFraxl :: forall f m a. DataSource f m => FreerT f m a -> m a
runFraxl = iterT $ \a -> runContT (run RNil a) snd where
  run :: Rec f q -> Ap f a' -> ContT x m (Rec m q, a')
  run list (Pure a) = lift $ (, a) <$> fetch list
  run list (Ap fa ff) = do
    (ma :& ms, f) <- run (fa :& list) ff
    lift $ fmap ((ms, ) . f) ma

simpleAsyncFetch :: forall m f q. MonadIO m
                    => (forall x. f x -> IO x)
                    -> Fetch f m q
simpleAsyncFetch fetchIO
  = rtraverse (fmap (liftIO . wait) . liftIO . async . fetchIO)

newtype CachedFetch f a = CachedFetch (f a)

instance ( DataSource f m
         , MonadTrans t
         , MonadState (DMap f MVar) (t m)
         , DMap.GCompare f
         , MonadIO (t m))
         => DataSource (CachedFetch f) (t m) where
  fetch list = runContT (snd <$> run RNil list) return where
    run :: Rec f q
        -> Rec (CachedFetch f) q'
        -> ContT x (t m) (Rec (t m) q, Rec (t m) q')
    run flist RNil = lift $ (, RNil) <$> lift (rmap lift <$> fetch flist)
    run flist (CachedFetch f :& fs) = do
      cache <- get
      case DMap.lookup f cache of
        Just mvar -> fmap (second (liftIO (readMVar mvar) :&)) (run flist fs)
        Nothing -> do
          (mvar :: MVar a) <- liftIO newEmptyMVar
          put (DMap.insert f mvar cache)
          let store :: t m a -> t m a
              store m = m >>= \a -> liftIO (putMVar mvar a) >> return a
          fmap (\(m :& ms, rest) -> (ms, store m :& rest)) (run (f :& flist) fs)

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
