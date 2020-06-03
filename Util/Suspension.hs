module Util.Suspension (
    Susp(..)
  , force
  ) where

data Susp a = S a

force :: Susp a -> a
force (S a) = a
