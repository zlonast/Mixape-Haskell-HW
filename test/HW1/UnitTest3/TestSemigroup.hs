module HW1.UnitTest3.TestSemigroup where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW1.Unit3.SemigroupVersion (Endo (Endo, unEndo), Name (Name), NonEmpty ((:|)),
                                   ThisOrThat (Both, That, This))

testsSemigroup :: SpecWith ()
testsSemigroup =
  describe "Task7 - Semigroup and Monoid" $ do
    testSemigroup
    testMonoid

testSemigroup :: SpecWith ()
testSemigroup = it "testSemigroup" $ do
    ("path" :| ["to"]) <> ("file" :| []) `shouldBe` ("path" :| ["to", "file"])
    This (1 :: Int) <> That (2 :: Int)   `shouldBe` Both (1 :: Int) (2 :: Int)

testMonoid :: SpecWith ()
testMonoid = it "testMonoid" $ do
    Name "root" <> Name "server" `shouldBe` Name "root.server"
    unEndo (Endo (4+) <> Endo (5+)) (4 :: Int) `shouldBe` 13



