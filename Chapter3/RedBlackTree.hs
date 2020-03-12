{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Chapter3.RedBlackTree (
  -- * Red-black tree type
    RedBlackTree (..)
  -- * Construction
  , empty
  -- ** From list
  , fromOrdList
  , toOrdList
  -- * Insertion
  , insert
  -- * Query
  -- ** Lookup
  , lookup
  , member
  -- ** Depth
  , minDepth
  , maxDepth
  ) where

import Prelude hiding (lookup)
import Data.Maybe (isJust)

-- | A red-black tree is a self balancing binary tree
-- in which every node is colored red or black.
--
-- Every red-black tree satisfy the following two balance invariants:
-- * No red node has a red child
-- * Every path from the root to an empty node contains the same number of black nodes.
--
-- Theorem. The maximum depth of a node in a red-black tree of size n is at most 2⌊log(n + 1)⌋
data RedBlackTree a
  = Bin Color !(RedBlackTree a) {-# UNPACK #-}!a !(RedBlackTree a)
  | Tip
  deriving (Functor, Foldable, Traversable)

type role RedBlackTree nominal

-- | Color of each node: Red and Black
data Color = R | B

-- | Returns an empty red-black tree.
empty :: RedBlackTree a
empty = Tip
{-# INLINEABLE empty #-}

-- | Return the element if it is present in the tree.
-- Otherwise, returns nothing.
--
-- Cost: O(log n)
lookup
  :: (Ord a)
  => a -> RedBlackTree a -> Maybe a
lookup = go
  where
    go _ Tip = Nothing
    go !x (Bin _ l !y r) =
      if x < y        then go x l
      else if x > y   then go x r
      else                 Just y
{-# INLINEABLE lookup #-}

-- | Checks if the element 'a' is present in the given red-black tree.
--
-- Cost: O(log n)
member
  :: (Ord a)
  => a -> RedBlackTree a -> Bool
member a = isJust . lookup a
{-# INLINEABLE member #-}

-- | Inserts an element while keeping the two invariants.
--
-- Cost: O(log n)
insert
  :: (Ord a)
  => a -> RedBlackTree a -> RedBlackTree a
insert !x tree =
  let (Bin _ l !y r) = ins tree
  in Bin B l y r
    where
      ins              Tip  = Bin R Tip x Tip
      ins node@(Bin _ l !y r) =
        if      x < y   then lbalance (Bin R (ins l) y r)
        else if x > y   then rbalance (Bin R l y (ins r))
        else                 node
{-# INLINEABLE insert #-}

-- TODO
-- delete
--   :: (Ord a)
--   => a -> RedBlackTree a -> RedBlackTree a
-- delete = undefined

-- | Balance the tree after a recursive insert on the left subtree.
--
-- Cost: O(1)
lbalance :: RedBlackTree a -> RedBlackTree a
lbalance (Bin B (Bin R (Bin R a !x b) !y c) !z d) = Bin R (Bin B a x b) y (Bin B c z d)
lbalance (Bin B (Bin R a !x (Bin R b !y c)) !z d) = Bin R (Bin B a x b) y (Bin B c z d)
lbalance tree                                  = tree
{-# INLINEABLE lbalance #-}


-- | Balance the tree after a recursive insert on the right subtree.
--
-- Cost: O(1)
rbalance :: RedBlackTree a -> RedBlackTree a
rbalance (Bin B a !x (Bin R b !y (Bin R c !z d))) = Bin R (Bin B a x b) y (Bin B c z d)
rbalance (Bin B a !x (Bin R (Bin R b !y c) !z d)) = Bin R (Bin B a x b) y (Bin B c z d)
rbalance tree                                  = tree
{-# INLINEABLE rbalance #-}

-- | Given an ordered list with no duplicate, returns a red-black tree.
--
-- Cost: O(n) (notice this is faster than n inserts.)
fromOrdList :: [a] -> RedBlackTree a
fromOrdList xs' =
  toTree (Tip, ins ([], xs'))
    where
      balance' :: [(Color, a, RedBlackTree a)] -> [(Color, a, RedBlackTree a)]
      balance'                              [(R, !v1, t1)] = [(B, v1, t1)]
      balance' ((R, !v1, t1):(R, !v2, t2):(B, !v3, t3):xs) = (B, v1, t1):(balance' ((R, v2, (Bin B t3 v3 t2)):xs))
      balance'                                       xs = xs

      -- ^^^^ Use the list of (color, value, left sub-tree) in order to avoid searching the insertion node.
      --      This list represents the right spine of the red-black tree from bottom to top.
      ins :: ( [(Color, a, RedBlackTree a)], [a] ) -> [(Color, a, RedBlackTree a)]
      ins (ts, [])   = ts
      ins (ts, !x:xs) = ins ( balance' ((R, x, Tip):ts), xs)

      toTree :: ( RedBlackTree a, [(Color, a, RedBlackTree a)] ) -> RedBlackTree a
      toTree (t, []) = t
      toTree (t, ((color, !v, t'):ts)) = toTree ((Bin color t' v t), ts)
      -- The amortized cost of ins is O(1), and ins is called n times.
      -- The complexity of toTree is O(log(n)).
      -- The total complexity is n*O(1) + O(log(n)) = O(n).
{-# INLINEABLE fromOrdList #-}

toOrdList :: RedBlackTree a -> [a]
toOrdList           Tip = []
toOrdList (Bin _ l !y r) = toOrdList l ++ [y] ++ toOrdList r
{-# INLINEABLE toOrdList #-}


depth :: (Int -> Int -> Int) -> RedBlackTree a -> Int
depth choice = go
  where
    go           Tip = 0
    go (Bin _ l _ r) = choice (minDepth l) (minDepth r) + 1
{-# INLINEABLE depth #-}

-- | Depth of the shortest path in the red-black tree from the root to the leaf.
--
-- Cost O(n)
minDepth :: RedBlackTree a -> Int
minDepth = depth min
{-# INLINEABLE minDepth #-}


-- | Depth of the largest path in the red-black tree from the root to the leaf.
--
-- Cost O(n)
maxDepth :: RedBlackTree a -> Int
maxDepth = depth max
{-# INLINEABLE maxDepth #-}
