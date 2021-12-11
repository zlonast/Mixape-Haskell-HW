{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
module HW1.UnitTest3.TestConcat where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW1.Unit3.Concat (eitherConcat, maybeConcat)


testsMaybe :: SpecWith ()
testsMaybe =
  describe "Task6 - Maybe and Either" $ do
    testMaybeConcat
    testEitherConcat

testMaybeConcat :: SpecWith ()
testMaybeConcat = it "testMaybeConcat" $ do
    maybeConcat @Int [] `shouldBe` []
    maybeConcat @Int [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5]
    maybeConcat @Char [Just "hello ", Nothing, Just "world"] `shouldBe` "hello world"

testEitherConcat :: SpecWith ()
testEitherConcat = it "testEitherConcat" $ do
    eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] `shouldBe` (Sum {unSum = 8}, [1 :: Int,2,3,4,5])
    eitherConcat [Left (Sum 3), Right (Product 4), Left (Sum 5), Right (Product 6)] `shouldBe` (Sum {unSum = 8}, Product {unProduct = 24})

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
