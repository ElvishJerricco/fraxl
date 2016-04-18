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
import           Control.Monad
import           Control.Monad.IO.Class
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
  fetch list = run RNil RNil list (fmap (\(_,_,x) -> x)) where
    run :: Rec f q
        -> Rec (Union r) q'
        -> Rec (Union (f ': r)) q''
        -> (m (Rec m q, Rec m q', Rec m q'') -> m x)
        -> m x
    run flist ulist RNil k = k $ (, , RNil) <$> fetch flist <*> fetch ulist
    run flist ulist (u :& us) k = case prj u of
      Right (fa :: f a) -> run (fa :& flist) ulist us k' where
        k' mlists = k $ fmap
          (\(ma :& ms, other, rest) -> (ms, other, ma :& rest))
          mlists
      Left u' -> run flist (u' :& ulist) us k' where
        k' mlists = k $ fmap
          (\(other, ma :& ms, rest) -> (other, ms, ma :& rest))
          mlists

runFraxl :: forall f m a. DataSource f m => FreerT f m a -> m a
runFraxl = iterT (run RNil (join . fmap snd)) where
  run :: Rec f q -> (m (Rec m q, a') -> m x) -> Ap f a' -> m x
  run list k (Pure a) = k $ (\l -> (l, a)) <$> fetch list
  run list k (Ap fa ff) = run (fa :& list) k' ff where
    k' m = do
      (ma :& ms, f) <- m
      a <- ma -- wait on async operations
      k $ return (ms, f a)

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
  fetch list = run RNil list (fmap snd) where
    run :: Rec f q
        -> Rec (CachedFetch f) q'
        -> (t m (Rec (t m) q, Rec (t m) q') -> t m x)
        -> t m x
    run flist RNil k = k $ (, RNil) <$> lift (rmap lift <$> fetch flist)
    run flist (CachedFetch f :& fs) k = do
      cache <- get
      case DMap.lookup f cache of
        Just mvar -> run flist fs k' where
          k' mlists = k $ fmap
            (second (liftIO (readMVar mvar) :&))
            mlists
        Nothing -> do
          mvar <- liftIO newEmptyMVar
          put (DMap.insert f mvar cache)
          run (f :& flist) fs (k' mvar) where
            k' (mvar :: MVar a) mlists = k $ fmap
              (\(m :& ms, rest) -> (ms, (m >>= putReturn) :& rest))
              mlists where
                putReturn :: a -> t m a
                putReturn a = liftIO (putMVar mvar a) >> return a

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
