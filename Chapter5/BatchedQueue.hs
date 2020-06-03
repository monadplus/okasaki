{-# LANGUAGE Strict #-}
module Chapter5.BatchedQueue where

import           StrictList (List(..))
import qualified StrictList as SL

data Queue a = Queue (List a) (List a)

empty :: Queue a
empty = Queue Nil Nil

isEmpty :: Queue a -> Bool
isEmpty (Queue Nil _) = True
isEmpty (Queue _ _) = False

checkf :: Queue a -> Queue a
checkf (Queue Nil r) = Queue (SL.reverse r) Nil
checkf q = q

snoc :: a -> Queue a -> Queue a
snoc x (Queue f r) = checkf (Queue f (Cons x r))

head :: Queue a -> a
head (Queue Nil _) = error "empty"
head (Queue (Cons x _) _) = x

tail :: Queue a -> Queue a
tail (Queue Nil _) = error "empty"
tail (Queue (Cons _ f) r) = checkf (Queue f r)
