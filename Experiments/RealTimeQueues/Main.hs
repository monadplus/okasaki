{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import qualified Chapter7.RealTimeQueue as RTQ
import           Control.DeepSeq
import           Gauge
import           Prelude                hiding (tail)
import           Test.QuickCheck


main :: IO ()
main =
  defaultMain

runBench :: Int -> Benchmark
runBench n

    [ env (generate $ genQueue @Int size) $ \q ->
        bgroup (show size)"10000"
          [ bench "snoc" $ whnf (RTQ.snoc 0) q
          , bench "head" $ whnf RTQ.head q
          , bench "tail" $ whnf RTQ.tail q
          ]
    ]


-- | Returns a Queue of the given size.
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

-- main :: IO ()
-- main =
--   defaultMain
--     [ bgroup "worst-case"
--         [ bench "snoc" $ benchQueue @Int (return . RTQ.snoc 0)
--         , bench "head" $ benchQueue @Int (return . RTQ.head)
--         , bench "snoc" $ benchQueue @Int (return . RTQ.tail)
--         ]
--     ]

-- Check if this is ok.
-- benchQueue
--   :: (Arbitrary a, NFData a, NFData b)
--   => (RTQ.Queue a -> IO b)
--   -> Benchmarkable
-- benchQueue = perBatchEnv (const (generate $ genQueue size))
