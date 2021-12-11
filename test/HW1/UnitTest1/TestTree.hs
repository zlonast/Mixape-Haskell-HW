{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
module HW1.UnitTest1.TestTree where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NL

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW1.Unit1.Tree (Tree (Leaf, Node), erase, fromList, insert, isEmpty, search, size)

testsTree :: SpecWith ()
testsTree =
  describe "Task3 - Binary Tree NonEmpty" $ do
    testEmpty
    testSize
    testSearch
    testInsert
    testFromList
    testErase

testEmpty :: SpecWith ()
testEmpty = it "testEmpty" $ do
    isEmpty Leaf `shouldBe` True
    isEmpty (Node Leaf ((1 :: Int) NL.:| []) Leaf) `shouldBe` False

testSize :: SpecWith ()
testSize = it "testSize" $ do
    size Leaf `shouldBe` 0
    size (Node Leaf ((1 :: Int) NL.:| []) Leaf) `shouldBe` 1
    size (Node (Node Leaf ((1 :: Int) NL.:| []) Leaf) (2 NL.:| []) Leaf) `shouldBe` 2
    size (Node Leaf ((1 :: Int) NL.:| []) (Node Leaf (2 NL.:| []) Leaf)) `shouldBe` 2
    size (Node (Node Leaf ((1 :: Int) NL.:| []) Leaf) (2 NL.:| []) (Node Leaf (3 NL.:| []) Leaf)) `shouldBe` 3
    size (fromList @Int [1, 2, 3, 4, 5]) `shouldBe` 5

testSearch :: SpecWith ()
testSearch = it "testSearch" $ do
    search @Int 0 example0 `shouldBe` False
    search @Int 1 example1 `shouldBe` True
    search @Int 2 example2 `shouldBe` True
    search @Int 3 example3 `shouldBe` False
    search @Int 4 example4 `shouldBe` False
    search @Int 5 (fromList [1, 2, 3, 4, 5]) `shouldBe` True
    search @Int 0 (fromList [1, 2, 3, 4, 5]) `shouldBe` False
    search @Int 6 (fromList [1, 2, 3, 4, 5]) `shouldBe` False where
        example0 = Leaf
        example1 = Node Leaf (1 NL.:| []) Leaf
        example2 = Node (Node Leaf (1 NL.:| []) Leaf) (2 NL.:| []) (Node Leaf (3 NL.:| []) Leaf)
        example3 = Node Leaf (1 NL.:| []) Leaf
        example4 = Node Leaf (1 NL.:| []) Leaf

testInsert :: SpecWith ()
testInsert = it "testInsert" $ do
    invariantBinatyTree (insert (6 :: Int) (fromList [1 .. 5])) `shouldBe` True
    TestTree (insert (6 :: Int) (fromList [1 .. 5])) `shouldBe` TestTree (fromList [1 .. 6])
    invariantBinatyTree (insert (7 :: Int) (fromList [1 .. 6])) `shouldBe` True
    TestTree (insert (7 :: Int) (fromList [1 .. 6])) `shouldBe` TestTree (fromList [1 .. 7])
    invariantBinatyTree (insert (8 :: Int) (fromList [1 .. 7])) `shouldBe` True
    TestTree (insert (8 :: Int) (fromList [1 .. 7])) `shouldBe` TestTree (fromList [1 .. 8])
    invariantBinatyTree (insert (9 :: Int) (fromList [1 .. 8])) `shouldBe` True
    TestTree (insert (9 :: Int) (fromList [1 .. 8])) `shouldBe` TestTree (fromList [1 .. 9])
    invariantBinatyTree (insert (1 :: Int) Leaf) `shouldBe` True
    TestTree (insert (1 :: Int) Leaf) `shouldBe` TestTree (Node Leaf (1 NL.:| []) Leaf)
    invariantBinatyTree (insert (1 :: Int) example2) `shouldBe` True
    TestTree (insert (1 :: Int) example2) `shouldBe` TestTree example1 where
        example1 = Node Leaf (1 NL.:| [1, 1, 1]) Leaf
        example2 = Node Leaf (1 NL.:| [1, 1]) Leaf

testFromList :: SpecWith ()
testFromList = it "testFromList" $ do
    invariantBinatyTree (fromList @Int []) `shouldBe` True
    invariantBinatyTree (fromList @Int [1]) `shouldBe` True
    invariantBinatyTree (fromList @Int [1, 2]) `shouldBe` True
    invariantBinatyTree (fromList @Int [1, 2, 3]) `shouldBe` True
    invariantBinatyTree (fromList @Int [1, 1, 1]) `shouldBe` True
    invariantBinatyTree (fromList @Int [1, 1, 2, 2]) `shouldBe` True
    TestTree example0  `shouldBe` TestTree (fromList @Int [])
    TestTree example1  `shouldBe` TestTree (fromList @Int [1])
    TestTree example2  `shouldBe` TestTree (fromList @Int [2, 1])
    TestTree example3  `shouldBe` TestTree (fromList @Int [1, 3, 2])
    TestTree example4  `shouldBe` TestTree (fromList @Int [1, 1, 1])
    TestTree example5  `shouldBe` TestTree (fromList @Int [1, 2, 1, 2, 2]) where
        example0 = Leaf
        example1 = Node Leaf (1 NL.:| []) Leaf
        example2 = Node (Node Leaf (1 NL.:| []) Leaf) (2 NL.:| []) Leaf
        example3 = Node (Node Leaf (1 NL.:| []) Leaf) (2 NL.:| []) (Node Leaf (3 NL.:| []) Leaf)
        example4 = Node Leaf (1 NL.:| [1, 1]) Leaf
        example5 = Node (Node Leaf (1 NL.:| [1]) Leaf) (2 NL.:| [2, 2]) Leaf

testErase :: SpecWith ()
testErase = it "testErase" $ do
    invariantBinatyTree (erase (5 :: Int) (fromList [1 .. 5])) `shouldBe` True
    TestTree (erase (5 :: Int) (fromList [1 .. 5])) `shouldBe` TestTree (fromList [1 .. 4])
    invariantBinatyTree (erase (7 :: Int) (fromList [1 .. 6])) `shouldBe` True
    TestTree (erase (7 :: Int) (fromList [1 .. 6])) `shouldBe` TestTree (fromList [1 .. 6])
    invariantBinatyTree (erase (2 :: Int) (fromList [1 .. 7])) `shouldBe` True
    TestTree (erase (2 :: Int) (fromList [1 .. 7])) `shouldBe` TestTree (fromList (1 : [3 .. 7]))
    invariantBinatyTree (erase (7 :: Int) (fromList [1 .. 8])) `shouldBe` True
    TestTree (erase (7 :: Int) (fromList [1 .. 8])) `shouldBe` TestTree (fromList ([1 .. 6] ++ [8]))
    invariantBinatyTree (erase (1 :: Int) Leaf) `shouldBe` True
    TestTree (erase (1 :: Int) Leaf) `shouldBe` TestTree Leaf
    invariantBinatyTree (erase (1 :: Int) example2) `shouldBe` True
    TestTree (erase (1 :: Int) example2) `shouldBe` TestTree example1 where
        example1 = Node Leaf (1 NL.:| [1, 1])    Leaf
        example2 = Node Leaf (1 NL.:| [1, 1, 1]) Leaf

invariantBinatyTree :: Ord a => Tree a -> Bool
invariantBinatyTree Leaf = True
invariantBinatyTree (Node Leaf nl Leaf) = invarintNode nl
invariantBinatyTree (Node l@(Node _ (n0 NL.:| _) _) nl@(n NL.:| _) Leaf) =
    (n0 < n) && invarintNode nl && invariantBinatyTree l
invariantBinatyTree (Node Leaf nl@(n NL.:| _) l@(Node _ (n0 NL.:| _) _)) =
    (n0 > n) && invarintNode nl && invariantBinatyTree l
invariantBinatyTree (Node l@(Node _ (n0 NL.:| _) _) nl@(n NL.:| _) r@(Node _ (n1 NL.:| _) _)) =
    (n0 < n && n < n1) && invarintNode nl && invariantBinatyTree l && invariantBinatyTree r

invarintNode :: Eq a => NL.NonEmpty a -> Bool
invarintNode = (== 1) . L.length . L.group . NL.toList

newtype TestTree a = TestTree { unTestTree :: Tree a } deriving stock Show

instance Eq a => Eq (TestTree a) where
    (==) :: Eq a => TestTree a -> TestTree a -> Bool
    (TestTree a) == (TestTree b) = toList a == toList b -- becacuse when we insert, we have different structure vs Balance fromList

-- no Foldable toList
toList :: Tree a -> [a]
toList Leaf          = []
toList (Node l nl r) = toList l ++ NL.toList nl ++ toList r
