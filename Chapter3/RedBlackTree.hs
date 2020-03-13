{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE RoleAnnotations    #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Chapter3.RedBlackTree
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
--
--
-- A red-black tree is a self balancing binary tree
-- in which every node is colored red or black.
--
-- Every red-black tree satisfy the following two balance invariants:
--
--    * No red node has a red child
--    * Every path from the root to an empty node contains the same number of black nodes.
--
-- These invariants are preserved on every insertion by a recursive rebalance.
--
-- Time complexity:
--
-- +-----------+---------------+------------+
-- | Algorithm | Average Case  | Worst Case |
-- +===========+===============+============+
-- | Space     | O(n)          | O(log n)   |
-- +-----------+---------------+------------+
-- | Search    | O(log n)      | O(log n)   |
-- +-----------+---------------+------------+
-- | Insert    | O(log n)      | O(log n)   |
-- +-----------+---------------+------------+
-- | Deete     | O(log n)      | O(log n)   |
-- +-----------+---------------+------------+
-------------------------------------------------------------------------------
module Chapter3.RedBlackTree (
  -- * Red-black tree type
    RedBlackTree (..)
  -- ** Color
  , Color(..)
  -- ** Invariants
  , InvariantException(..)
  -- * Construction
  , empty
  -- ** From list
  , fromList
  , fromOrdList
  , toOrdList
  -- * Insertion
  , insert
  -- * Delete
  , delete
  -- * Query
  -- ** Lookup
  , lookup
  , member
  -- ** Depth
  , depth
  , minDepth
  , maxDepth
  -- ** Invariants Check
  , checkInvariants
  -- * Testing
  , genRBT
  ) where

--------------------------------------------------------------------------

import           Control.Exception
import           Data.Maybe        (isJust)
import           Prelude           hiding (lookup)
import           Test.QuickCheck
import qualified Data.List as List

--------------------------------------------------------------------------

-- | Red-black Tee data type
data RedBlackTree a
  = Bin Color !(RedBlackTree a) !a !(RedBlackTree a)
  | Tip
  deriving (Functor, Foldable, Traversable)

instance Show a => Show (RedBlackTree a) where
  show = drawTree

type role RedBlackTree nominal

-- | Color of each node.
data Color = R -- ^ Red
           | B -- ^ Black
  deriving (Show, Enum)

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
    go x (Bin _ l y r) =
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
insert x tree =
  let (Bin _ a y b) = ins tree
  in Bin B a y b
    where
      ins               Tip  = Bin R Tip x Tip
      ins node@(Bin c a y b) =
        if      x < y   then lbalance (Bin c (ins a) y b)
        else if x > y   then rbalance (Bin c a y (ins b))
        else                 node
{-# INLINEABLE insert #-}

-- | Balance the tree after a recursive insert on the left subtree.
--
-- Cost: O(1)
lbalance :: RedBlackTree a -> RedBlackTree a
lbalance (Bin B (Bin R (Bin R a x b) y c) z d) = Bin R (Bin B a x b) y (Bin B c z d)
lbalance (Bin B (Bin R a x (Bin R b y c)) z d) = Bin R (Bin B a x b) y (Bin B c z d)
lbalance tree                                  = tree
{-# INLINEABLE lbalance #-}


-- | Balance the tree after a recursive insert on the right subtree.
--
-- Cost: O(1)
rbalance :: RedBlackTree a -> RedBlackTree a
rbalance (Bin B a x (Bin R b y (Bin R c z d))) = Bin R (Bin B a x b) y (Bin B c z d)
rbalance (Bin B a x (Bin R (Bin R b y c) z d)) = Bin R (Bin B a x b) y (Bin B c z d)
rbalance tree                                  = tree
{-# INLINEABLE rbalance #-}


delete
 :: (Ord a)
 => a -> RedBlackTree a -> RedBlackTree a
delete = error "Not implemented."
{-# INLINEABLE delete #-}

-- | Construct a red-black tree from an arbitrary list.
--
-- Cost: O(n*log(n))
--
-- If you **know** that the list is sorted, use 'fromOrdList'.
fromList :: (Ord a) => [a] -> RedBlackTree a
fromList = foldr insert empty

-- | Given an ordered list with no duplicate, returns a red-black tree.
--
-- @
-- fromOrdList [1.10000]
-- @
--
-- Cost: O(n) (notice this is faster than n inserts.)
fromOrdList :: [a] -> RedBlackTree a
fromOrdList xs' =
  toTree (Tip, ins ([], xs'))
    where
      balance' :: [(Color, a, RedBlackTree a)] -> [(Color, a, RedBlackTree a)]
      balance'                              [(R, v1, t1)] = [(B, v1, t1)]
      balance'   ((R, v1, t1):(R, v2, t2):(B, v3, t3):xs) = (B, v1, t1):(balance' ((R, v2, (Bin B t3 v3 t2)):xs))
      balance'                                         xs = xs

      -- ^^^^ Use the list of (color, value, left sub-tree) in order to avoid searching the insertion node.
      --      This list represents the right spine of the red-black tree from bottom to top.
      ins :: ( [(Color, a, RedBlackTree a)], [a] ) -> [(Color, a, RedBlackTree a)]
      ins (ts, [])   = ts
      ins (ts, x:xs) = ins ( balance' ((R, x, Tip):ts), xs)

      toTree :: ( RedBlackTree a, [(Color, a, RedBlackTree a)] ) -> RedBlackTree a
      toTree (t, [])                  = t
      toTree (t, ((color, v, t'):ts)) = toTree ((Bin color t' v t), ts)
      -- The amortized cost of ins is O(1), and ins is called n times.
      -- The complexity of toTree is O(log(n)).
      -- The total complexity is n*O(1) + O(log(n)) = O(n).
{-# INLINEABLE fromOrdList #-}

-- | Given a red-black tree, returns an ordered list with no duplicates.
toOrdList :: RedBlackTree a -> [a]
toOrdList           Tip = []
toOrdList (Bin _ l y r) = toOrdList l ++ [y] ++ toOrdList r
{-# INLINEABLE toOrdList #-}

-- | The depth of a node is the number of edges from the node to the root.
--
-- 'heigth' and 'depth' are equivalent in this context.
--
-- The heigth and depth are properties of the nodes, not of the trees.
-- e.g. on a tree of size 3, a node of heigth 2 has depth 0, and viceversa.
depth :: (Int -> Int -> Int) -> RedBlackTree a -> Int
depth choice = (subtract 1) . go -- ^ The root has depth 0
  where
    go           Tip = 0
    go (Bin _ l _ r) = choice (go l) (go r) + 1
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

data InvariantException
    = Invariant1Exception -- ^ No red node has a red child
    | Invariant2Exception -- ^ Every path from the root to an empty node contains the same number of black nodes.
  deriving (Show, Exception)

-- | Every red-black tree satisfy the following two balance invariants:
--   * No red node has a red child
--   * Every path from the root to an empty node contains the same number of black nodes.
checkInvariants :: RedBlackTree a -> Bool
checkInvariants t = go B 0 t
  where
    blackNodes = countBlackNodes t -- arbitrary branch

    bothRed R R = True
    bothRed _ _ = False

    go _ acc Tip = acc == blackNodes
    go c1 !acc (Bin c2 l _ r)
      | bothRed c1 c2 = False
      | otherwise =
          let l' = go c2 (acc + fromEnum c2) l
              r' = go c2 (acc + fromEnum c2) r
          in l' && r'
{-# INLINEABLE checkInvariants #-}


-- | Count the black nodes of the right most path.
countBlackNodes :: RedBlackTree a -> Int
countBlackNodes = go 0
  where
    go !acc Tip           = acc
    go !acc (Bin c _ _ r) = go (acc + fromEnum c) r
{-# INLINEABLE countBlackNodes #-}


-- | Neat 2-dimensional drawing of a tree.
drawTree :: (Show a) => RedBlackTree a -> String
drawTree  = unlines . draw

-- | Inner drawing of a tree
draw :: (Show a) => RedBlackTree a -> [String]
draw Tip = ["x"]
draw (Bin c l x r) =
  (show x ++ "[" ++ show c ++ "]") : drawSubTrees [l, r]
    where
      shift first other =
        zipWith (++) (first : repeat other)

      drawSubTrees []     = []
      drawSubTrees [t]    = "|" : shift "l- " "   " (draw t)
      drawSubTrees (t:ts) = "|" : shift "r- " "|  " (draw t) ++ drawSubTrees ts


-- | A generator for values of type 'RedBlackTree' of the given size.
genRBT :: (Arbitrary a, Ord a) => Int -> Gen (RedBlackTree a)
genRBT = fmap (fromList . unUnique) . genUniqueList -- Otherwise the size would't be n

newtype UniqueList a = UniqueList { unUnique :: [a] }
    deriving Show

-- | 90 % of samples are randomly distributed elements
--   10 % are sorted.
genUniqueList :: (Arbitrary a, Ord a) => Int -> Gen (UniqueList a)
genUniqueList n =
  frequency [ (9, genUniqueList' n arbitrary)
            , (1, (UniqueList . unSorted) <$> genUniqueSortedList n arbitrary)
            ]

genUniqueList' :: (Eq a) => Int -> Gen a -> Gen (UniqueList a)
genUniqueList' n gen =
  UniqueList <$> vectorOf n gen `suchThat` isUnique

newtype UniqueSortedList a = UniqueSortedList { unSorted :: [a] }
    deriving Show

genUniqueSortedList :: (Ord a) => Int -> Gen a -> Gen (UniqueSortedList a)
genUniqueSortedList n gen =
  UniqueSortedList . List.sort . unUnique <$> genUniqueList' n gen

isUnique :: Eq a => [a] -> Bool
isUnique x = List.nub x == x
