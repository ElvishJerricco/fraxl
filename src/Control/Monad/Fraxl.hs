module Control.Monad.Fraxl
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
  -- * Fraxl Monads
  , MonadFraxl(..)
  ) where

import           Control.Monad.Fraxl.Class
import           Control.Monad.Trans.Fraxl
import           Data.GADT.Compare
