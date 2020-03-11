{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RoleAnnotations #-}
module Chapter2.BST (
    module Chapter2.BST
  ) where

-----------------------------------------------------------

import Data.Maybe (isJust)

-----------------------------------------------------------

data BST a = Bin !(BST a) {-# UNPACK #-}!a !(BST a)
           | Tip

type role BST nominal

empty :: BST a
empty = Tip
{-# INLINEABLE empty #-}

lookup :: (Ord a) => a -> BST a -> Maybe a
lookup = go
  where
    go _ Tip = Nothing
    go x (Bin l c r) =
      if x < c
        then go x l
      else if x > c
        then go x r
      else
        Just c
{-# INLINEABLE lookup #-}

member
  :: (Ord a)
  => a -> BST a -> Bool
member a = isJust . Chapter2.BST.lookup a
{-# INLINEABLE member #-}

insert
  :: (Ord a)
  => a -> BST a -> BST a
insert = go
  where
    go !x Tip =
      Bin Tip x Tip
    go x b@(Bin l c r) =
      if x < c
        then Bin (go x l) c r
      else if x > c
        then Bin l c (go x r)
      else
        b
{-# INLINEABLE insert #-}


bst :: BST Int
bst =
  insert 1 $
  insert 2 $
  insert 3 $
  insert 4 $
  insert 5 $
  empty

main :: IO ()
main = do
  putStrLn $ "Does it contain 3? " ++ show (member 3 bst)
  putStrLn $ "Does it contain 6? " ++ show (member 6 bst)

----------------------------------------------------------

type Map k v = BST (KeyValue k v)

newtype KeyValue k v = KeyValue { pair :: (k,v) }
  deriving Eq

instance (Ord k, Eq v) => Ord (KeyValue k v) where
  (KeyValue (k1, _)) `compare` (KeyValue (k2, _)) = k1 `compare` k2

empty' :: Map k v
empty' = empty

bind :: (Ord k, Eq v) => k -> v -> Map k v -> Map k v
bind key value = insert (KeyValue (key, value))

lookup' :: (Ord k) => k -> Map k v -> Maybe v
lookup' = go
  where
    go _ Tip = Nothing
    go k (Bin l (KeyValue (k1, v)) r) =
      if k < k1
        then go k l
      else if k > k1
        then go k r
      else
        Just v
{-# INLINEABLE lookup' #-}
