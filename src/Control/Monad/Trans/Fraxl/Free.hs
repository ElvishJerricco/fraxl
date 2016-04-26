{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Trans.Fraxl.Free
  (
  -- * The base functor
    FreeF(..)
  -- * The free monad transformer
  , FreeT(..)
  -- * The free monad
  , Free
  -- * Operations
  , liftF
  , iterT
  , iterTM
  , hoistFreeT
  , transFreeT
  , joinFreeT
  , retractT
  -- * Operations of free monad
  , retract
  , iter
  , iterM
  -- * Free Monads With Class
  , MonadFree(..)
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Cont.Class
import           Control.Monad.Error.Class
import           Control.Monad.Free.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.Writer.Class
import           Data.Functor.Identity
import           Data.Monoid
import           Data.TASequence.FastCatQueue

-- Commented here is the simplest definition of this version of the Free monad.
-- It's a freer monad relying on Applicative for optimization.
--------------------------------------------------------------------------------
-- data Free f a where
--   Pure :: a -> Free f a
--   Impure :: f a -> (a -> Free f b) -> Free f b
--
-- instance Functor (Free f) where
--   fmap f (Pure a) = Pure (f a)
--   fmap f (Impure a k) = Impure a (fmap f . k)
--
-- instance Applicative f => Applicative (Free f) where
--   pure = Pure
--   Pure f <*> a = fmap f a
--   Impure x k <*> Pure a = Impure x (fmap ($ a) . k)
--   Impure x k <*> Impure y k' = Impure (fmap ((<*>) . k) x <*> fmap k' y) id
--
-- instance Applicative f => Monad (Free f) where
--   Pure a >>= k = k a
--   Impure x k' >>= k = Impure x (k' >=> k)
--------------------------------------------------------------------------------

-- Arrows for type-aligned.
-- http://okmij.org/ftp/Haskell/zseq.pdf
type Arr f m = Kleisli (FreeT f m)

(>.<) :: (Applicative m, TASequence s)
      => (FreeT f m b -> FreeT f m c)
      -> s (Arr f m) a b
      -> s (Arr f m) a c
(>.<) f arrs = case tviewr arrs of
  TAEmptyR -> tsingleton $ Kleisli (f . freePure)
  ks :> Kleisli ar -> ks |> Kleisli (f . ar)

qApp :: (Applicative f, Monad m, TASequence s)
     => s (Arr f m) a b
     -> a -> FreeT f m b
qApp arrs a = case tviewl arrs of
  TAEmptyL -> pure a
  Kleisli k :< ks -> k a >>= qApp ks

-- | The base functor for a free monad.
data FreeF f m a where
  Pure :: a -> FreeF f m a
  Free :: f b -> FastTCQueue (Arr f m) b a -> FreeF f m a
instance Applicative m => Functor (FreeF f m) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Free b k) = Free b (fmap f >.< k)
  {-# INLINE fmap #-}

transFreeF :: (Applicative f, Monad m)
           => (forall x. f x -> g x)
           -> FreeF f m a
           -> FreeF g m a
transFreeF _ (Pure a) = Pure a
transFreeF t (Free b k) = Free (t b) k' where
  k' = tmap (Kleisli . (transFreeT t .) . runKleisli) k
{-# INLINE transFreeF #-}

-- | The \"free monad transformer\" for an applicative functor @f@
newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f m a) }

instance Applicative m => Functor (FreeT f m) where
  fmap f (FreeT m) = FreeT $ fmap (fmap f) m
  {-# INLINE fmap #-}

-- Applicative 'pure' but with no @Applicative f@ constraint
freePure :: Applicative m => a -> FreeT f m a
freePure = FreeT . pure . Pure
{-# INLINE freePure #-}

instance (Applicative f, Monad m) => Applicative (FreeT f m) where
  pure = freePure
  {-# INLINE pure #-}
  FreeT f <*> FreeT a = FreeT $ g <$> f <*> a where
    g :: FreeF f m (a -> b) -> FreeF f m a -> FreeF f m b
    g (Pure f') a' = fmap f' a'
    g (Free b kf) (Pure a') = Free b (fmap ($ a') >.< kf)
    g (Free b kf) (Free c ka) = Free (f' <$> b <*> c) (tsingleton (Kleisli id))
      where f' b' c' = qApp kf b' <*> qApp ka c'
  {-# INLINE (<*>) #-}

instance (Applicative f, Monad m) => Monad (FreeT f m) where
  FreeT ma >>= k = FreeT $ do
    freef <- ma
    case freef of
      Pure a -> runFreeT (k a)
      Free b k' -> return $ Free b (k' |> Kleisli k)
  {-# INLINE (>>=) #-}

instance MonadTrans (FreeT f) where
  lift = FreeT . fmap Pure

instance (Applicative f, Monad m) => MonadFree f (FreeT f m) where
  wrap = FreeT . return . flip Free (tsingleton $ Kleisli id)
  {-# INLINE wrap #-}

instance (Applicative f, MonadIO m) => MonadIO (FreeT f m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (Applicative f, MonadReader r m) => MonadReader r (FreeT f m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f = hoistFreeT (local f)
  {-# INLINE local #-}

instance (Applicative f, MonadWriter w m) => MonadWriter w (FreeT f m) where
  tell = lift . tell
  {-# INLINE tell #-}
  listen (FreeT m) = FreeT $ concat' <$> listen (relisten <$> m)
    where
      relisten (Pure a) = Pure (a, mempty)
      relisten (Free y ks) = Free y (listen >.< ks)
      concat' (Pure (x, w1), w2) = Pure (x, w1 <> w2)
      concat' (Free x ks, w) = Free x $ fmap (second (w <>)) >.< ks
  pass m = FreeT . pass' . runFreeT . hoistFreeT clean $ listen m
    where
      clean = pass . fmap (\x -> (x, const mempty))
      pass' = join . fmap g
      g (Pure ((x, f), w)) = tell (f w) >> return (Pure x)
      g (Free x ks)        = return $ Free x $ (FreeT . pass' . runFreeT) >.< ks
  writer w = lift (writer w)
  {-# INLINE writer #-}

instance (Applicative f, MonadState s m) => MonadState s (FreeT f m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
  state f = lift (state f)
  {-# INLINE state #-}

instance (Applicative f, MonadError e m) => MonadError e (FreeT f m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  FreeT m `catchError` f = FreeT $ fmap recatch m `catchError` (runFreeT . f)
    where recatch (Pure x) = Pure x
          recatch (Free x ks) = Free x $ (`catchError` f) >.< ks

instance (Applicative f, MonadCont m) => MonadCont (FreeT f m) where
  callCC f = FreeT $ callCC (\k -> runFreeT $ f (lift . k . Pure))

instance (Applicative f, MonadPlus m) => Alternative (FreeT f m) where
  empty = FreeT mzero
  FreeT ma <|> FreeT mb = FreeT (mplus ma mb)
  {-# INLINE (<|>) #-}

instance (Applicative f, MonadPlus m) => MonadPlus (FreeT f m) where
  mzero = FreeT mzero
  {-# INLINE mzero #-}
  mplus (FreeT ma) (FreeT mb) = FreeT (mplus ma mb)
  {-# INLINE mplus #-}

instance (Applicative f, MonadThrow m) => MonadThrow (FreeT f m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

instance (Applicative f, MonadCatch m) => MonadCatch (FreeT f m) where
  FreeT m `catch` f = FreeT $ fmap recatch m `catch` (runFreeT . f)
    where recatch (Pure x) = Pure x
          recatch (Free x ks) = Free x $ (`catch` f) >.< ks
  {-# INLINE catch #-}

-- | Tear down a free monad transformer using iteration.
iterT :: (Applicative f, Monad m) => (f (m a) -> m a) -> FreeT f m a -> m a
iterT f (FreeT m) = do
    val <- m
    case val of
        Pure x -> return x
        Free y k -> f $ fmap (iterT f . qApp k) y

-- | Tear down a free monad transformer using iteration over a transformer.
iterTM :: ( Applicative f
          , Monad m
          , MonadTrans t
          , Monad (t m))
          => (f (t m a) -> t m a) -> FreeT f m a -> t m a
iterTM f (FreeT m) = do
    val <- lift m
    case val of
        Pure x -> return x
        Free y k -> f $ fmap (iterTM f . qApp k) y

-- | Lift a monad homomorphism from @m@ to @n@ into a monad homomorphism from @'FreeT' f m@ to @'FreeT' f n@
--
-- @'hoistFreeT' :: ('Monad' m, 'Functor' f) => (m ~> n) -> 'FreeT' f m ~> 'FreeT' f n@
hoistFreeT :: (Monad m, Applicative f)
           => (forall a. m a -> n a)
           -> FreeT f m b
           -> FreeT f n b
hoistFreeT mh = FreeT . mh . fmap f . runFreeT where
  f (Pure a) = Pure a
  f (Free b k) = Free b $ tmap (Kleisli . (hoistFreeT mh .) . runKleisli) k

-- | Lift a natural transformation from @f@ to @g@ into a monad homomorphism from @'FreeT' f m@ to @'FreeT' g m@
transFreeT :: (Applicative f, Monad m)
           => (forall a. f a -> g a)
           -> FreeT f m b
           -> FreeT g m b
transFreeT nt = FreeT . fmap (transFreeF nt) . runFreeT

-- | Pull out and join @m@ layers of @'FreeT' f m a@.
joinFreeT :: forall m f a. ( Monad m
                           , Traversable f
                           , Applicative f)
                           => FreeT f m a -> m (Free f a)
joinFreeT (FreeT m) = m >>= joinFreeF
  where
    joinFreeF :: FreeF f m a -> m (Free f a)
    joinFreeF (Pure x) = return (return x)
    joinFreeF (Free y ks) = wrap <$> mapM (joinFreeT . qApp ks) y

-- | Tear down a free monad transformer using Monad instance for @t m@.
retractT :: (MonadTrans t, Monad (t m), Monad m) => FreeT (t m) m a -> t m a
retractT (FreeT m) = do
  val <- lift m
  case val of
    Pure x -> return x
    Free y k -> y >>= retractT . qApp k

-- | The \"free monad\" for an applicative functor @f@.
type Free f = FreeT f Identity

-- |
-- 'retract' is the left inverse of 'liftF'
--
-- @
-- 'retract' . 'liftF' = 'id'
-- @
retract :: Monad f => Free f a -> f a
retract m =
  case runIdentity (runFreeT m) of
    Pure a  -> return a
    Free x ks -> x >>= retract . qApp ks

-- | Tear down a 'Free' 'Monad' using iteration.
iter :: Applicative f => (f a -> a) -> Free f a -> a
iter phi = runIdentity . iterT (Identity . phi . fmap runIdentity)

-- | Like 'iter' for monadic values.
iterM :: (Applicative f, Monad m) => (f (m a) -> m a) -> Free f a -> m a
iterM phi = iterT phi . hoistFreeT (return . runIdentity)
