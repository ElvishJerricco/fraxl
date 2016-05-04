{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main where

import ExampleDataSource
import Control.Monad.Fraxl
import Control.Monad
import Data.Time.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Printf

main :: IO ()
main = do
    [test,n_] <- getArgs
    let n = read n_
    t0 <- getCurrentTime
    case test of
      -- parallel, identical queries
      "par1" -> evalCachedFraxl (fetchExample |:| fNil) $
        void $ sequenceA (replicate n (listWombats 3 :: Fraxl '[ExampleReq] IO [Id]))
      -- parallel, distinct queries
      "par2" -> evalCachedFraxl (fetchExample |:| fNil) $
        void $ sequenceA (map listWombats [1..fromIntegral n] :: [Fraxl '[ExampleReq] IO [Id]])
      -- sequential, identical queries
      "seqr" -> evalCachedFraxl (fetchExample |:| fNil) $
        foldr andThen (return ()) (replicate n (listWombats 3 :: Fraxl '[ExampleReq] IO [Id]))
      -- sequential, left-associated, distinct queries
      "seql" -> evalCachedFraxl (fetchExample |:| fNil) $
        void $ foldl andThen (return []) (map listWombats [1.. fromIntegral n] :: [Fraxl '[ExampleReq] IO [Id]])
      "tree" -> evalCachedFraxl (fetchExample |:| fNil) $ void (tree n :: Fraxl '[ExampleReq] IO [Id])
      _ -> do
        hPutStrLn stderr "syntax: monadbench par1|par2|seqr|seql NUM"
        exitWith (ExitFailure 1)
    t1 <- getCurrentTime
    printf "%d reqs: %.2fs\n" n (realToFrac (t1 `diffUTCTime` t0) :: Double)
  where
    -- can't use >>, it is aliased to *> and we want the real bind here
    andThen x y = x >>= const y

tree :: MonadFraxl ExampleReq m => Int -> m [Id]
tree 0 = listWombats 0
tree n = concat <$> sequenceA
  [ tree (n-1)
  , listWombats (fromIntegral n), tree (n-1)
  ]
