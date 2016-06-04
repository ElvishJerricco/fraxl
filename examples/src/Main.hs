{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Concurrent
import           Control.Monad.Fraxl
import           Control.Monad.IO.Class
import           Control.Monad.State

main :: IO ()
main = do
  let fraxl = (++) <$> myFraxl <*> myFraxl
  (strs, reqs) <- runStateT (evalCachedFraxl (fetchMySource |:| fetchMySource2 |:| fetchNil) fraxl) 0
  putStrLn ("Number of MySource2 requests made: " ++ show reqs)
  print $ length strs
  print strs

myFraxl :: (MonadFraxl MySource m, MonadFraxl MySource2 m) => m [String]
myFraxl = replicate <$> myInt2 <*> myString

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

fetchMySource :: MonadIO m => Fetch MySource m a
fetchMySource = simpleAsyncFetch simpleFetch where
  simpleFetch :: MySource a -> IO a
  simpleFetch MyString = do
    putStrLn "Sleeping String!"
    threadDelay 1000000
    return "String!"
  simpleFetch MyInt = do
    putStrLn "Sleeping Int!"
    threadDelay 1000000
    return 10

myString :: MonadFraxl MySource m => m String
myString = dataFetch MyString

myInt :: MonadFraxl MySource m => m Int
myInt = dataFetch MyInt

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

fetchMySource2 :: (MonadIO m, MonadState Int m) => Fetch MySource2 m a
fetchMySource2 a = modify (+ clength a) >> simpleAsyncFetch simpleFetch a where
  clength :: ASeq f r -> Int
  clength ANil = 0
  clength (ACons _ rs) = 1 + clength rs
  simpleFetch :: MySource2 a -> IO a
  simpleFetch MyString2 = do
    putStrLn "Sleeping String2!"
    threadDelay 1000000
    return "String!"
  simpleFetch MyInt2 = do
    putStrLn "Sleeping Int2!"
    threadDelay 1000000
    return 10

myString2 :: MonadFraxl MySource2 m => m String
myString2 = dataFetch MyString2

myInt2 :: MonadFraxl MySource2 m => m Int
myInt2 = dataFetch MyInt2
