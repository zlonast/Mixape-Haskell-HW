module HW1.UnitTest1.TestDay (testsWeeksDay) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW1.Unit1.Day (Day (Monday, Sunday, Tuesday), afterDays, daysToParty, isWeekend, nextDay)


testsWeeksDay :: SpecWith ()
testsWeeksDay =
  describe "Task1 - Sweet Summer Days" $ do
    testDaysToParty
    testIsWeekend
    testNextDay
    testAfterDays

testDaysToParty :: SpecWith ()
testDaysToParty = it "testDaysToParty" $ do
    map daysToParty [Monday .. Sunday] `shouldBe` [4, 3, 2, 1, 0, 6, 5]

testIsWeekend :: SpecWith ()
testIsWeekend = it "testIsWeekend" $ do
    map isWeekend [Monday .. Sunday] `shouldBe` [False, False, False, False, False, True, True]

testNextDay :: SpecWith ()
testNextDay = it "testNextDay" $ do
    map nextDay [Monday .. Sunday] `shouldBe` [Tuesday .. Sunday] ++ [Monday]

testAfterDays :: SpecWith ()
testAfterDays = it "testAfterDays" $ do
    [afterDays x y | x <- [Monday .. Sunday], y <- [0 .. 6]]
    `shouldBe` [toEnum ((x + y) `mod` 7) | x <- [0..6], y <- [0..6]]
