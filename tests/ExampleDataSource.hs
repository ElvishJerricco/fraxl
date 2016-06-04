{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module ExampleDataSource (
    -- * requests for this data source
    Id(..), ExampleReq(..)
  , fetchExample
  , countAardvarks
  , listWombats
  ) where

import           Control.Monad.Fraxl

-- Here is an example minimal data source.  Our data source will have
-- two requests:
--
--   countAardvarks :: String -> Haxl Int
--   listWombats    :: Id     -> Haxl [Id]
--
-- First, the data source defines a request type, with one constructor
-- for each request:

newtype Id = Id Int
  deriving (Eq, Ord, Enum, Num, Integral, Real)

instance Show Id where
  show (Id i) = show i

data ExampleReq a where
  CountAardvarks :: String -> ExampleReq Int
  ListWombats    :: Id     -> ExampleReq [Id]

-- The request type (ExampleReq) is parameterized by the result type of
-- each request.  Each request might have a different result, so we use a
-- GADT - a data type in which each constructor may have different type
-- parameters. Here CountAardvarks is a request that takes a String
-- argument and its result is Int, whereas ListWombats takes an Id
-- argument and returns a [Id].

deriving instance Show (ExampleReq a)

instance GEq ExampleReq where
  CountAardvarks _ `geq` CountAardvarks _ = Just Refl
  ListWombats _ `geq` ListWombats _ = Just Refl
  _ `geq` _ = Nothing

instance GCompare ExampleReq where
  CountAardvarks a `gcompare` CountAardvarks b = case a `compare` b of
    EQ -> GEQ
    LT -> GLT
    GT -> GGT
  ListWombats a `gcompare` ListWombats b = case a `compare` b of
    EQ -> GEQ
    LT -> GLT
    GT -> GGT
  CountAardvarks _ `gcompare` ListWombats _ = GLT
  ListWombats _ `gcompare` CountAardvarks _ = GGT

-- We need to define an instance of DataSource:

fetchExample :: Monad m => Fetch ExampleReq m a
fetchExample ANil = return ANil
fetchExample (ACons (CountAardvarks str) rs) = ACons <$> return (return (length (filter (== 'a') str))) <*> fetchExample rs
fetchExample (ACons (ListWombats a) rs) = ACons <$> return (return (take (fromIntegral a) [1..])) <*> fetchExample rs

countAardvarks :: MonadFraxl ExampleReq m => String -> m Int
countAardvarks str = dataFetch (CountAardvarks str)

listWombats :: MonadFraxl ExampleReq m => Id -> m [Id]
listWombats a = dataFetch (ListWombats a)
