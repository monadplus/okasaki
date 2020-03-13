{-# LANGUAGE TypeApplications #-}
module Experiment (
    runExperiment
  ) where

---------------------------------------------------

import qualified Chapter3.RedBlackTree as RBT
import qualified Control.Foldl         as L
import           Control.Monad
import           Test.QuickCheck

---------------------------------------------------

-- |
-- 1.- Generate a random vector of [n-m, n+m] elements
-- 2.- Transform it into a red-black tree.
-- 3.- Compute the maximum length
-- 4.- Get the statistics
-- 5.- Output them on the stdout
runExperiment
  :: Int -- ^ Number of samples
  -> Int -- ^ Size of the samples
  -> IO ()
runExperiment n size = do
  samples <- generate $ replicateM n (RBT.genRBT @Int size)
  let (Just max', mean, std) = L.fold ((,,) <$> L.maximum <*> L.mean <*> L.std) $ fmap (fromIntegral . RBT.maxDepth) samples
  report n size max' mean std

-- | Write a report in the stdout.
report
  :: Int -- ^ Number of samples
  -> Int -- ^ Size of the samples
  -> Double -- ^ Max
  -> Double -- ^ Mean
  -> Double -- ^ Std
  -> IO ()
report n size max' mean std = do
  putStrLn "---------------------------------"
  putStrLn $ "# samples: " ++ show n
  putStrLn $ "tree size: " ++ show size
  putStrLn ""
  putStrLn $ "max depth(max) : " ++ show max'
  putStrLn $ "max depth(mean): " ++ show mean
  putStrLn $ "max depth(std) : " ++ show std
  putStrLn ""
  putStrLn $ "max depth  (theoretical): " ++ show (theoreticalMaxDepth size)
  putStrLn $ "perfectly balanced depth: " ++ show (perfectlyBalanced size)
  putStrLn "---------------------------------"

theoreticalMaxDepth
  :: Int -- ^ Size of the red-black tree
  -> Int
theoreticalMaxDepth size =
  2 * floor (logBase 2 (fromIntegral (size + 1)) :: Double)

perfectlyBalanced
  :: Int -- ^ Size of the red-black tree
  -> Int
perfectlyBalanced size =
  floor (logBase 2 (fromIntegral size) :: Double)
