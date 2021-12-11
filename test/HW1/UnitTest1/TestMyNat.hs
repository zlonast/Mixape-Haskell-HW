{-# LANGUAGE TypeApplications #-}
module HW1.UnitTest1.TestMyNat where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (Testable (property))

import HW1.Unit1.MyNat (Nat (..), isEven)

testsMyNat :: SpecWith ()
testsMyNat =
  describe "Task2 - My Nats" $ do
    testPlusNat
    testMulNat
    testMinusNat
    testEnumNat
    testEqNat
    testEvenNat
    testDivNat
    testModNat
    testPlusNatProperty
    testMulNatProperty
    testMinusNatProperty
    testEnumNatProperty
    testEqNatProperty
    testEvenNatProperty
    testDivNatProperty
    testModNatProperty

testPlusNat :: SpecWith ()
testPlusNat = it "testPlusNat" $ do
    Z + Z                 `shouldBe` Z
    Z + S Z               `shouldBe` S Z
    S (S Z) + S (S (S Z)) `shouldBe` S (S (S (S (S Z))))
    S Z + Z               `shouldBe` S Z
    S (S (S (S Z))) + Z   `shouldBe` S (S (S (S Z)))
    toEnum 5 + toEnum 11  `shouldBe` toEnum @Int 16

testPlusNatProperty :: SpecWith ()
testPlusNatProperty = it "testPlusNatProperty" $ property $
    \ x y -> toEnum x + toEnum y `shouldBe` toEnum @Int (x + y)

testMulNat :: SpecWith ()
testMulNat = it "testMulNat" $ do
    Z * Z                 `shouldBe` Z
    Z * S Z               `shouldBe` Z
    S (S Z) * S (S (S Z)) `shouldBe` S (S (S (S (S (S Z)))))
    S Z * Z               `shouldBe` Z
    S (S (S (S Z))) * Z   `shouldBe` Z
    toEnum 5 * toEnum 11  `shouldBe` toEnum @Int 55

testMulNatProperty :: SpecWith ()
testMulNatProperty = it "testMulNatProperty" $ property $
    \ x y -> toEnum x * toEnum y `shouldBe` toEnum @Int (x * y)

testMinusNat :: SpecWith ()
testMinusNat = it "testMulNat" $ do
    Z - Z                 `shouldBe` Z
    Z - S Z               `shouldBe` Z
    S (S (S Z)) - S (S Z) `shouldBe` S Z
    S Z - Z               `shouldBe` S Z
    S (S (S (S Z))) - Z   `shouldBe` S (S (S (S Z)))
    toEnum 11 - toEnum 5  `shouldBe` toEnum @Int 6

testMinusNatProperty :: SpecWith ()
testMinusNatProperty = it "testMinusNatProperty" $ property $
    \ x y -> toEnum x - toEnum y `shouldBe` toEnum @Int (x - y)

testEnumNat :: SpecWith ()
testEnumNat = it "testEnumNat" $ do
    (fromInteger . toInteger) Z                 `shouldBe` (toEnum @Int . fromEnum) Z
    (fromInteger . toInteger) (S Z)             `shouldBe` (toEnum @Int . fromEnum) (S Z)
    (fromInteger . toInteger) (S (S (S Z)))     `shouldBe` (toEnum @Int . fromEnum) (S (S (S Z)))
    (fromInteger . toInteger) (S (S Z))         `shouldBe` (toEnum @Int . fromEnum) (S (S Z))
    (fromInteger . toInteger) (S (S (S (S Z)))) `shouldBe` (toEnum @Int . fromEnum) (S (S (S (S Z))))
    (fromInteger . toInteger @Int) (toEnum 11)       `shouldBe` toEnum @Int 11

testEnumNatProperty :: SpecWith ()
testEnumNatProperty = it "testEnumNatProperty" $ property $
    \ x -> (fromInteger . toInteger) (toEnum @Int x) `shouldBe` toEnum @Int x

testEqNat :: SpecWith ()
testEqNat = it "testEqNat" $ do
    Z == Z                    `shouldBe` True
    Z == S Z                  `shouldBe` False
    S (S (S Z)) == S (S Z)    `shouldBe` False
    S Z == S Z                `shouldBe` True
    S (S (S (S Z))) == Z      `shouldBe` False
    toEnum 5 == toEnum @Nat 5 `shouldBe` True

testEqNatProperty :: SpecWith ()
testEqNatProperty = it "testEqNatProperty" $ property $
    \ x y -> abs x == abs y `shouldBe` toEnum x == toEnum @Nat y

testEvenNat :: SpecWith ()
testEvenNat = it "testEvenNat" $ do
    isEven Z                  `shouldBe` True
    isEven (S Z)              `shouldBe` False
    isEven (S (S (S Z)))      `shouldBe` False
    isEven (S (S Z))          `shouldBe` True
    isEven (S (S (S (S Z))))  `shouldBe` True
    isEven (toEnum @Nat 5)    `shouldBe` False

testEvenNatProperty :: SpecWith ()
testEvenNatProperty = it "testEvenNatProperty" $ property $
    \ x -> isEven (toEnum @Nat x) `shouldBe` even x

testDivNat :: SpecWith ()
testDivNat = it "testDivNat" $ do
    Z `div` Z                     `shouldBe` Z
    Z `div` S Z                   `shouldBe` Z
    S (S (S Z)) `div` S Z         `shouldBe` S (S (S Z))
    S (S (S (S Z))) `div` S (S Z) `shouldBe` S (S Z)
    S (S (S (S Z))) `div` S Z     `shouldBe` S (S (S (S Z)))
    toEnum @Int 18 `div` toEnum 3      `shouldBe` toEnum 6

testDivNatProperty :: SpecWith ()
testDivNatProperty = it "testDivNatProperty" $ property $
    \ x y -> toEnum x `div` S (toEnum y) `shouldBe` toEnum (abs x `div` (abs y + 1))

testModNat :: SpecWith ()
testModNat = it "testModNat" $ do
    Z `mod` Z                     `shouldBe` Z
    Z `mod` S Z                   `shouldBe` Z
    S (S (S Z)) `mod` S (S Z)     `shouldBe` S Z
    S (S (S (S Z))) `mod` S (S Z) `shouldBe` Z
    S Z             `mod` S (S Z) `shouldBe` S Z
    toEnum @Int 19 `mod` toEnum 4      `shouldBe` toEnum 3

testModNatProperty :: SpecWith ()
testModNatProperty = it "testModNatProperty" $ property $
    \ x y -> toEnum x `mod` S (toEnum y) `shouldBe` toEnum (abs x `mod` (abs y + 1))











