module HW2.UnitTest2.TestEvalExpr where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW2.Unit2.EvalExpr (ArithmeticError (..), Expr (..), eval)

testsEvalExpr :: SpecWith ()
testsEvalExpr =
  describe "Task4 - eval Expr or ArithmeticError" $ do
    testVal
    testPlus
    testMinus
    testMul
    testDiv
    testUp
    testMix

testVal :: SpecWith ()
testVal = it "testVal" $ do
  Val 5    `shouldBe` Val 5
  Val 0    `shouldBe` Val 0
  Val (-1) `shouldBe` Val (-1)

testPlus :: SpecWith ()
testPlus = it "testPlus" $ do
  eval (Val 5 :+: Val 5)       `shouldBe` Right 10
  eval (Val (-5) :+: Val 5)    `shouldBe` Right 0
  eval (Val (-8) :+: Val 5)    `shouldBe` Right (-3)
  eval (Val (-8) :+: Val (-3)) `shouldBe` Right (-11)

testMinus :: SpecWith ()
testMinus = it "testMinus" $ do
  eval (Val 5 :-: Val 5)       `shouldBe` Right 0
  eval (Val (-5) :-: Val 5)    `shouldBe` Right (-10)
  eval (Val (-3) :-: Val (-5)) `shouldBe` Right 2

testMul :: SpecWith ()
testMul = it "testMul" $ do
  eval (Val 5 :*: Val 5)       `shouldBe` Right 25
  eval (Val 5 :*: Val 0)       `shouldBe` Right 0
  eval (Val 0 :*: Val 5)       `shouldBe` Right 0
  eval (Val 2 :*: Val 5)       `shouldBe` Right 10
  eval (Val (-3) :*: Val 5)    `shouldBe` Right (-15)
  eval (Val (-4) :*: Val (-7)) `shouldBe` Right 28

testDiv :: SpecWith ()
testDiv = it "testDiv" $ do
  eval (Val 5 :/: Val 5)       `shouldBe` Right 1
  eval (Val 5 :/: Val 0)       `shouldBe` Left DivByZero
  eval (Val 0 :/: Val 0)       `shouldBe` Left DivByZero
  eval (Val (-3) :/: Val 0)    `shouldBe` Left DivByZero
  eval (Val 4 :/: Val 5)       `shouldBe` Right 0
  eval (Val (-4) :/: Val 5)    `shouldBe` Right (-1)
  eval (Val (-4) :/: Val (-2)) `shouldBe` Right 2
  eval (Val 17 :/: Val (-2))   `shouldBe` Right (-9)

testUp :: SpecWith ()
testUp = it "testUp" $ do
  eval (Val 2 :^: Val 2)       `shouldBe` Right 4
  eval (Val 5 :^: Val 3)       `shouldBe` Right 125
  eval (Val (-3) :^: Val 3)    `shouldBe` Right (-27)
  eval (Val 0 :^: Val 2)       `shouldBe` Right 0
  eval (Val 23 :^: Val 0)      `shouldBe` Right 1
  eval (Val 2 :^: Val (-3))    `shouldBe` Left NegExpon
  eval (Val (-5) :^: Val (-3)) `shouldBe` Left NegExpon

testMix :: SpecWith ()
testMix = it "testMix" $ do
  eval (Val 5 :+: Val 5 :-: Val 5) `shouldBe` Right 5
  eval (Val 5 :+: Val 5 :*: Val 2) `shouldBe` Right 15
  eval (Val 5 :-: Val 5 :-: Val 5) `shouldBe` Right (-5)
  eval (Val 2 :*: Val 2 :-: Val 5) `shouldBe` Right (-1)
  eval (Val 5 :+: Val 5 :/: Val 5) `shouldBe` Right 6
  eval (Val 5 :*: Val 2 :*: Val 3) `shouldBe` Right 30
  eval (Val 5 :*: Val 2 :^: Val 3) `shouldBe` Right 40
  eval (Val 5 :/: Val 0 :^: Val 3) `shouldBe` Left DivByZero
  eval (Val 5 :^: Val (-1) :^: Val 3) `shouldBe` Left NegExpon
  eval (Val 5 :*: Val 2 :-: Val 3) `shouldBe` Right 7
  eval (Val 5 :/: Val 2 :^: Val 3) `shouldBe` Right 0


