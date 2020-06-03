module Chapter6.BankersQueue where

data Queue a =
  Queue {-# UNPACK #-} !Int -- Length of f
        [a]                 -- f
        {-# UNPACK #-} !Int -- Length of r
        [a]                 -- r

empty :: Queue a
empty = Queue 0 [] 0 []

isEmpty :: Queue a -> Bool
isEmpty (Queue lenf _ _ _) = lenf == 0

check :: Queue a -> Queue a
check q@(Queue lenf f lenr r)
  | lenr <= lenf = q
  | otherwise    = Queue (lenf + lenr) (f ++ reverse r) 0 []

snoc :: a -> Queue a -> Queue a
snoc x (Queue lenf f lenr r) = check (Queue lenf f (lenr + 1) (x:r))

head :: Queue a -> a
head (Queue _ [] _ _) = error "empty"
head (Queue _ (x:_) _ _) = x

tail :: Queue a -> Queue a
tail (Queue _ [] _ _) = error "empty"
tail (Queue lenf (_:f') lenr r) = check (Queue (lenf - 1) f' lenr r)
