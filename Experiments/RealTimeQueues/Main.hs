{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import qualified Chapter7.RealTimeQueue as RTQ
import qualified Data.Foldable          as Foldable
import qualified Data.List              as List
import           Gauge
import           Prelude                hiding (tail)
import           Test.QuickCheck

-- Recall that gauge do not have a pretty html output.

main :: IO ()
main = defaultMain
  [ bgroup "Real-Time Queues"
      [ runBench 1000
      , runBench 10000
      , runBench 100000
      ]
  ]

runBench :: Int -> Benchmark
runBench size =
  env (generate $ genQueue @Int size) $ \q ->
    bgroup (show size)
      [ bench "snoc" $ whnf (applyN 100 (RTQ.snoc 0)) q
      --, bench "head" $ whnf RTQ.head q
      , bench "tail" $ whnf (applyN 100 RTQ.tail) q
      ]

-- | Returns a Queue of the given size.
--
-- FIXME: generating queues of size > 1000000 is slow.
genQueue
  :: Arbitrary a
  => Int
  -> Gen (RTQ.Queue a)
genQueue n
  | n < 0     = error "Size must be positive"
  | n == 0    = return RTQ.empty
  | otherwise = go 0 RTQ.empty
  where
    go m q
      | n == m    = return q
      | m == 0    = snoc q >>= go 1
      | otherwise = do
          (q', m') <- frequency [ (4, (,m + 1) <$> snoc q)
                                , (1, (,m - 1) <$> tail q)
                                ]
          go m' q'

    snoc q' = (`RTQ.snoc` q') <$> arbitrary

    tail q' = pure $ RTQ.tail q'

applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) id (List.replicate n f)
