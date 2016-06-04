{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Not actually undecidable.
-- @MonadFraxl f (Fraxl r m)@ just doesn't include the unnecessary @i@.
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Fraxl.Class
 (
 -- * Fraxl Monads
   MonadFraxl(..)
 ) where

import           Control.Applicative.Free.Fast
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Fraxl
import           Control.Monad.Trans.Free.Ap
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.List
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy      as Lazy
import qualified Control.Monad.Trans.RWS.Strict    as Strict
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Data.Vinyl.Optic.Plain.Class
import qualified Data.Vinyl.Prelude.CoRec          as CR
import           Data.Vinyl.Types

-- | Class for Fraxl-capable monads.
class Monad m => MonadFraxl f m where
  -- | 'dataFetch' is used to make a request of type 'f'.
  dataFetch :: f a -> m a

instance (Monad m, RElem f r i) => MonadFraxl f (Fraxl r m) where
  dataFetch = liftF . liftAp . Union . FunctorCoRec . CR.lift . Flap

instance Monad m => MonadFraxl f (FreerT f m) where
  dataFetch = liftF . liftAp

instance MonadFraxl f m => MonadFraxl f (ContT r m) where
  dataFetch = lift . dataFetch

instance MonadFraxl f m => MonadFraxl f (ExceptT e m) where
  dataFetch = lift . dataFetch

instance MonadFraxl f m => MonadFraxl f (IdentityT m) where
  dataFetch = lift . dataFetch

instance MonadFraxl f m => MonadFraxl f (ListT m) where
  dataFetch = lift . dataFetch

instance MonadFraxl f m => MonadFraxl f (MaybeT m) where
  dataFetch = lift . dataFetch

instance MonadFraxl f m => MonadFraxl f (ReaderT e m) where
  dataFetch = lift . dataFetch

instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Lazy.RWST r w s m) where
  dataFetch = lift . dataFetch

instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Strict.RWST r w s m) where
  dataFetch = lift . dataFetch

instance MonadFraxl f m => MonadFraxl f (Lazy.StateT s m) where
  dataFetch = lift . dataFetch

instance MonadFraxl f m => MonadFraxl f (Strict.StateT s m) where
  dataFetch = lift . dataFetch

instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Lazy.WriterT w m) where
  dataFetch = lift . dataFetch

instance (MonadFraxl f m, Monoid w) => MonadFraxl f (Strict.WriterT w m) where
  dataFetch = lift . dataFetch
