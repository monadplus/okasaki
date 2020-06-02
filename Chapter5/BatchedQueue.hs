{-# LANGUAGE Strict #-}
module Chapter5.BatchedQueue where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue f _) = null f

checkf :: Queue a -> Queue a
checkf (Queue [] r) = Queue (reverse r) []
checkf q = q

snoc :: a -> Queue a -> Queue a
snoc x (Queue f r) = checkf (Queue f (x:r))

head :: Queue a -> a
head (Queue [] _) = error "empty"
head (Queue (x:_) _) = x

tail :: Queue a -> Queue a
tail (Queue [] _) = error "empty"
tail (Queue (_:f) r) = checkf (Queue f r)
