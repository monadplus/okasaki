module Main where

----------------------------------------

import qualified Experiment

----------------------------------------

main :: IO ()
main = do
  -- ^ Run the RBT max depth experiment
  Experiment.runExperiment 100 100
