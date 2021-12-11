{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
module HW1.UnitTest2.TestFoldTree where

import qualified Data.List as L

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (Testable (property))

import HW1.Unit1.Tree (fromList)
import HW1.Unit2.FoldTree (toList)



testsFoldTree :: SpecWith ()
testsFoldTree =
  describe "Task4 - Foldable Tree" $ do
    testFoldr
    testFoldMap
    testFoldrPropertySum
    testFoldrPropertyProduct
    testFoldMapPropertySum
    testFoldMapPropertyProduct
    testToListFromListSortedProperty

testToListFromListSortedProperty :: SpecWith ()
testToListFromListSortedProperty = it "testToListFromListSortedProperty" $ property $
    \ x -> (toList . fromList @Int) x `shouldBe` L.sort x

testFoldr :: SpecWith ()
testFoldr = it "testFoldr" $ do
    foldr (+) 1 (fromList @Int [1, 2, 3]) `shouldBe` 7
    foldr (*) 2 (fromList @Int [1, 2, 3]) `shouldBe` 12
    foldr (-) 1 (fromList @Int [1, 2, 3]) `shouldBe` 1

testFoldMap :: SpecWith ()
testFoldMap = it "testFoldMap" $ do
    foldMap Sum  (fromList @Int [1, 2, 3, 4, 5]) `shouldBe` Sum 15
    foldMap Product (fromList @Int [1, 2, 3, 4]) `shouldBe` Product 24

testFoldrPropertySum :: SpecWith ()
testFoldrPropertySum = it "testFoldrPropertySum" $ property $
    \ x -> foldr (+) 1 (fromList @Int x) `shouldBe` sum x + 1

testFoldrPropertyProduct :: SpecWith ()
testFoldrPropertyProduct = it "testFoldrPropertyProduct" $ property $
    \ x -> foldr (*) 2 (fromList @Int x) `shouldBe` product x * 2

testFoldMapPropertySum :: SpecWith ()
testFoldMapPropertySum = it "testFoldMapPropertySum" $ property $
    \ x -> foldMap Sum (fromList x) `shouldBe` Sum (sum x)

testFoldMapPropertyProduct :: SpecWith ()
testFoldMapPropertyProduct = it "testFoldMapPropertyProduct" $ property $
    \ x -> foldMap Product (fromList x) `shouldBe` Product (product x)

newtype Sum = Sum {unSum :: Int} deriving stock (Show, Eq)

instance Semigroup Sum where
    (Sum a) <> (Sum b) = Sum (a + b)

instance Monoid Sum where
    mempty = Sum 0

newtype Product = Product {unProduct :: Int} deriving stock (Show, Eq)

instance Semigroup Product where
    (Product a) <> (Product b) = Product (a * b)

instance Monoid Product where
    mempty = Product 1




