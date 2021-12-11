{-# LANGUAGE TypeApplications #-}
module HW1.UnitTest2.TestSplit where

import qualified Data.List.NonEmpty as NL

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (Testable (property))

import HW1.Unit2.Split (joinWith, splitOn)


testsSplit :: SpecWith ()
testsSplit =
  describe "Task5 - Split and Join" $ do
    testSplitOn
    testJoinWith
    testSplitOnJoinWithId
    testSplitOnJoinWithIdProperty

testSplitOn :: SpecWith ()
testSplitOn = it "testSplitOn" $ do
    splitOn '/' "path/to/file" `shouldBe` "path" NL.:| ["to", "file"]
    splitOn ' ' "matlog tt haskell" `shouldBe` "matlog" NL.:| ["tt", "haskell"]

testJoinWith :: SpecWith ()
testJoinWith = it "testJoinWith" $ do
    joinWith '/' ("path" NL.:| ["to", "file"]) `shouldBe` "path/to/file"
    joinWith ' ' ("matlog" NL.:| ["tt", "haskell"]) `shouldBe` "matlog tt haskell"

testSplitOnJoinWithId :: SpecWith ()
testSplitOnJoinWithId = it "testSplitOnJoinWithId" $ do
    (joinWith '/' . splitOn '/')  "path/to/file" `shouldBe` "path/to/file"
    (joinWith ' ' . splitOn ' ')  "matlog tt haskell" `shouldBe` "matlog tt haskell"

testSplitOnJoinWithIdProperty :: SpecWith ()
testSplitOnJoinWithIdProperty = it "testSplitOnJoinWithIdProperty" $ property $
    \ x y -> (joinWith @Char y . splitOn y) x `shouldBe` x
