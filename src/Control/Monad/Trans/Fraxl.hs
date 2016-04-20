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

module Control.Monad.Trans.Fraxl where

import           Control.Applicative.Free.Fast
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Free.Ap   hiding (Pure)
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DMap
import           Data.Dependent.OpenUnion

type FreerT f = FreeT (Ap f)
type Fraxl r = FreerT (Union r)
type Fetch f m a = ASeq f a -> m (ASeq m a)

class Monad m => DataSource f m where
  fetch :: Fetch f m q

instance Monad m => DataSource (Union '[]) m where
  fetch ANil = return ANil
  fetch (ACons _ _) = error "Not possible - empty union"

instance ( DataSource f m
         , DataSource (Union r) m)
         => DataSource (Union (f ': r)) m where
  fetch list = (\(_, _, x) -> x) <$> run ANil ANil list where
    run :: ASeq f a
        -> ASeq (Union r) a'
        -> ASeq (Union (f ': r)) a''
        -> m (ASeq m a, ASeq m a', ASeq m a'')
    run flist ulist ANil = (, , ANil) <$> fetch flist <*> fetch ulist
    run flist ulist (ACons u us) = case prj u of
      Right (fa :: f a) -> fmap
        (\(ACons ma ms, other, rest) -> (ms, other, ACons ma rest))
        (run (ACons fa flist) ulist us)
      Left u' -> fmap
        (\(other, ACons ma ms, rest) -> (other, ms, ACons ma rest))
        (run flist (ACons u' ulist) us)

runFraxl :: forall f m a. DataSource f m => FreerT f m a -> m a
runFraxl = iterT $ \a -> unAp a (\f s -> join (reduceASeq <$> fetch s) >>= f) (const id) ANil

simpleAsyncFetch :: forall m f a. MonadIO m
                    => (forall x. f x -> IO x)
                    -> Fetch f m a
simpleAsyncFetch fetchIO
  = traverseASeq (fmap (liftIO . wait) . liftIO . async . fetchIO)

newtype CachedFetch f a = CachedFetch (f a)

instance ( DataSource f m
         , MonadTrans t
         , MonadState (DMap f MVar) (t m)
         , DMap.GCompare f
         , MonadIO (t m))
         => DataSource (CachedFetch f) (t m) where
  fetch list = snd <$> run ANil list where
    run :: ASeq f a
        -> ASeq (CachedFetch f) a'
        -> t m (ASeq (t m) a, ASeq (t m) a')
    run flist ANil = (, ANil) <$> lift (hoistASeq lift <$> fetch flist)
    run flist (ACons (CachedFetch f) fs) = do
      cache <- get
      case DMap.lookup f cache of
        Just mvar -> fmap (second (ACons (liftIO $ readMVar mvar))) (run flist fs)
        Nothing -> do
          (mvar :: MVar x) <- liftIO newEmptyMVar
          put (DMap.insert f mvar cache)
          let store :: t m x -> t m x
              store m = m >>= \a -> liftIO (putMVar mvar a) >> return a
          fmap (\(ACons m ms, rest) -> (ms, ACons (store m) rest)) (run (ACons f flist) fs)

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
