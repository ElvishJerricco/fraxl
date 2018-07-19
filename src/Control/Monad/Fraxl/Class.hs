{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Not actually undecidable.
-- @MonadFraxl f (Fraxl r m)@ is not undecidable,
-- but @f ∈ r@ doesn't satisfy the functional dependency @Fraxl r m -> f@.
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Fraxl.Class
 (
 -- * Fraxl Monads
   MonadFraxl(..)
 ) where

import           Control.Applicative.Fraxl.Free
import           Control.Monad.Free.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Fraxl
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy      as Lazy
import qualified Control.Monad.Trans.RWS.Strict    as Strict
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Data.Vinyl
import           Data.Vinyl.CoRec

-- | Class for Fraxl-capable monads.
class Monad m => MonadFraxl f m where
  -- | 'dataFetch' is used to make a request of type 'f'.
  dataFetch :: f a -> m a
  default dataFetch :: (MonadTrans t, MonadFraxl f n, t n ~ m) => f a -> m a
  dataFetch = lift . dataFetch

instance (Monad m, f ∈ r) => MonadFraxl f (Fraxl r m) where
  dataFetch = liftF . liftAp . Union . CoRec . Flap

instance Monad m => MonadFraxl f (FreerT f m) where
  dataFetch = liftF . liftAp

instance MonadFraxl f m => MonadFraxl f (ContT r m) where
instance MonadFraxl f m => MonadFraxl f (ExceptT e m) where
instance MonadFraxl f m => MonadFraxl f (IdentityT m) where
instance MonadFraxl f m => MonadFraxl f (MaybeT m) where
instance MonadFraxl f m => MonadFraxl f (ReaderT e m) where
instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Lazy.RWST r w s m) where
instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Strict.RWST r w s m) where
instance MonadFraxl f m => MonadFraxl f (Lazy.StateT s m) where
instance MonadFraxl f m => MonadFraxl f (Strict.StateT s m) where
instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Lazy.WriterT w m) where
instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Strict.WriterT w m) where
