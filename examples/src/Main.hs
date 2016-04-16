{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Concurrent
import           Control.Fraxl
import           Control.Fraxl.Class
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.GADT.Compare

main :: IO ()
main = do
  let fraxl :: Fraxl '[MySource, MySource2] (StateT Int IO) [String]
      fraxl = (++) <$> myFraxl <*> myFraxl
  (strs, reqs) <- runStateT (evalCachedFraxl fraxl) 0
  putStrLn ("Number of MySource2 requests made: " ++ show reqs)
  print $ length strs
  print strs

myFraxl :: (MonadFraxl MySource m, MonadFraxl MySource2 m) => m [String]
myFraxl = replicate <$> dataFetch MyInt2 <*> dataFetch MyString

data MySource a where
  MyString :: MySource String
  MyInt :: MySource Int

instance GEq MySource where
  MyString `geq` MyString = Just Refl
  MyInt `geq` MyInt = Just Refl
  _ `geq` _ = Nothing

instance GCompare MySource where
  MyString `gcompare` MyString = GEQ
  MyString `gcompare` MyInt = GLT
  MyInt `gcompare` MyString = GGT
  MyInt `gcompare` MyInt = GEQ

instance MonadIO m => DataSource MySource m where
  fetch = simpleAsyncFetch simpleFetch where
    simpleFetch :: MySource a -> IO a
    simpleFetch MyString = do
      putStrLn "Sleeping String!"
      threadDelay 1000000
      return "String!"
    simpleFetch MyInt = do
      putStrLn "Sleeping Int!"
      threadDelay 1000000
      return 10

data MySource2 a where
  MyString2 :: MySource2 String
  MyInt2 :: MySource2 Int

instance GEq MySource2 where
  MyString2 `geq` MyString2 = Just Refl
  MyInt2 `geq` MyInt2 = Just Refl
  _ `geq` _ = Nothing

instance GCompare MySource2 where
  MyString2 `gcompare` MyString2 = GEQ
  MyString2 `gcompare` MyInt2 = GLT
  MyInt2 `gcompare` MyString2 = GGT
  MyInt2 `gcompare` MyInt2 = GEQ

instance (MonadIO m, MonadState Int m) => DataSource MySource2 m where
  fetch a = modify (+ 1) >> simpleAsyncFetch simpleFetch a where
    simpleFetch :: MySource2 a -> IO a
    simpleFetch MyString2 = do
      putStrLn "Sleeping String2!"
      threadDelay 1000000
      return "String!"
    simpleFetch MyInt2 = do
      putStrLn "Sleeping Int2!"
      threadDelay 1000000
      return 10
