module Chapter6.PhysicistQueue where

import Control.DeepSeq

data Queue a =
  Queue ![a] -- w: copy of f (to prevent fully evaluating f)
        {-# UNPACK #-} !Int -- Length of f
        [a]  -- f (this should be suspended)
        {-# UNPACK #-} !Int -- Length of r
        ![a] -- r

empty :: Queue a
empty = Queue [] 0 [] 0 []

isEmpty :: Queue a -> Bool
isEmpty (Queue _ lenf _ _ _) = lenf == 0

check :: NFData a => Queue a -> Queue a
check q@(Queue _ lenf f lenr r)
  | lenr <= lenf = checkw q
  | otherwise = let f' = force f
                 in checkw (Queue f' (lenf + lenr) (f' ++ reverse r) 0 [])

checkw :: NFData a => Queue a -> Queue a
checkw (Queue [] lenf f lenr r) = Queue (force f) lenf f lenr r
checkw q = q

snoc :: NFData a => a -> Queue a -> Queue a
snoc x (Queue w lenf f lenr r) = check (Queue w lenf f (lenr + 1) (x:r))

head :: NFData a => Queue a -> a
head (Queue _ _ [] _ _) = error "empty"
head (Queue _ _ (x:_) _ _) = x

tail :: NFData a => Queue a -> Queue a
tail (Queue _ _ [] _ _) = error "empty"
tail (Queue (_:w') lenf f lenr r) = check (Queue w' (lenf - 1) (Prelude.tail $ force f) lenr r)
tail Queue{} = error "inconceivable" -- guaranteed by invariant, but not in the type system.
