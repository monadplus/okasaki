{-# LANGUAGE LambdaCase #-}
module Chapter7.RealTimeQueue where

import           StrictList (List(..))

data Queue a =
  Queue [a]       -- f
        (List a)  -- r
        [a]       -- Schedule

empty :: Queue a
empty = Queue [] Nil []

isEmpty :: Queue a -> Bool
isEmpty = \case
  Queue [] _ _ -> True
  _            -> False

-- | Incremental f ++ reverse r
rotate :: [a] -> List a -> [a]
rotate f1 r1 = rotate' f1 r1 []
  where
    rotate' :: [a] -> List a -> [a] -> [a]
    rotate' [] (Cons y _) a = y:a
    rotate' (x:xs) (Cons y ys) a = x : rotate' xs ys (y:a)
    rotate' _ _ _ = error "Inconceivable by invariant"

exec :: [a] -> List a -> [a] -> Queue a
exec f r (x:s) = x `seq` Queue f r s
exec f r [] =
  let f' = rotate f r
   in Queue f' Nil f'

snoc :: a -> Queue a -> Queue a
snoc x (Queue f r s) =
  let r' = Cons x r
   in exec f r' s

head :: Queue a -> a
head (Queue [] _ _)    = error "empty"
head (Queue (x:_) _ _) = x

tail :: Queue a -> Queue a
tail (Queue [] _ _)    = error "empty"
tail (Queue (_:f) r s) = exec f r s
