{-# LANGUAGE ScopedTypeVariables #-}
module Chapter3.RedBlackTreeSpec (spec) where

--------------------------------------------------

import           Chapter3.RedBlackTree (Color (..), RedBlackTree (..))
import qualified Chapter3.RedBlackTree as RBT
import           Control.Exception     (evaluate)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

--------------------------------------------------

spec :: Spec
spec = do
  describe "Red-black tree" $ do
    it "checkInvariants should check if invariants are fulfilled" $ do
      RBT.checkInvariants rbt1 `shouldBe` True
      RBT.checkInvariants bad  `shouldBe` False
      RBT.checkInvariants bad2 `shouldBe` False

    it "insert should preserve invariants" $ do
      let rbt = foldr RBT.insert RBT.empty ([1..10000] :: [Int])
      RBT.checkInvariants rbt `shouldBe` True

    it "fromOrdList should preserve invariants" $ do
      let rbt = RBT.fromOrdList [1..10000] :: RedBlackTree Int
      RBT.checkInvariants rbt `shouldBe` True

    it "toOrdList should return an ordered list of the elements of the tree" $ do
      let rbt = foldr RBT.insert RBT.empty ([3,5,2,1,8,9,5,4] :: [Int])
      RBT.toOrdList rbt `shouldBe` [1,2,3,4,5,8,9]

    it "toOrdList . fromOrdList == id" $ do
      let xs = [1..1000] :: [Int]
          f = RBT.toOrdList . RBT.fromOrdList
      f xs `shouldBe` xs

    prop "maxDepth = 2*floor[log (n + 1)]" $ do
      property $
        \(n :: Positive Int) ->
           let size = 1000
               rbt = RBT.fromOrdList [1..size] :: RedBlackTree Int
           in RBT.maxDepth rbt <= 2*floor (logBase 2.0 (fromIntegral size + 1.0))

------------------------------------------------------

rbt1 :: RedBlackTree Int
rbt1 = Bin B (Bin B (Bin R Tip 1 Tip) 3 Tip) 5 (Bin B (Bin R Tip 6 Tip) 10 Tip)

-- | Invariant 2 not fulfilled
bad :: RedBlackTree Int
bad = Bin B (Bin B (Bin B Tip 1 Tip) 3 Tip) 5 (Bin R (Bin B Tip 6 Tip) 10 Tip)

-- | Invariant 1 not fulfilled
bad2 :: RedBlackTree Int
bad2 = Bin B (Bin R (Bin R Tip 1 Tip) 3 Tip) 5 (Bin R (Bin B Tip 6 Tip) 10 Tip)
