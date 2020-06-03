module Chapter6.PhysicistQueue where

import           StrictList (List(..))
import qualified StrictList as SL

import           Util.Suspension

data Queue a =
  Queue (List a)              -- w
        {-# UNPACK #-} !Int -- Length of f
        (Susp (List a))       -- f
        {-# UNPACK #-} !Int -- Length of r
        (List a)              -- r

empty :: Queue a
empty = Queue Nil 0 (S Nil) 0 Nil

isEmpty :: Queue a -> Bool
isEmpty (Queue _ lenf _ _ _) = lenf == 0

checkw :: Queue a -> Queue a
checkw (Queue Nil lenf f lenr r) = Queue (force f) lenf f lenr r
checkw q = q

check :: Queue a -> Queue a
check q@(Queue _ lenf f lenr r)
  | lenr <= lenf = checkw q
  | otherwise = let f' = force f
                 in checkw (Queue f' (lenf + lenr) (S (f' <> SL.reverse r)) 0 Nil)

snoc :: a -> Queue a -> Queue a
snoc x (Queue w lenf f lenr r) = check (Queue w lenf f (lenr + 1) (Cons x r))

head :: Queue a -> a
head (Queue Nil _ _ _ _) = error "empty"
head (Queue (Cons x _) _ _ _ _) = x

tail :: Queue a -> Queue a
tail (Queue Nil _ _ _ _) = error "empty"
tail (Queue (Cons _ w') lenf f lenr r) = check (Queue w' (lenf - 1) (S (SL.tail (force f))) lenr r)
