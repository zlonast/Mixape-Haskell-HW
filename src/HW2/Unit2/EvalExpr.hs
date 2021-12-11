{-# LANGUAGE DerivingStrategies #-}
module HW2.Unit2.EvalExpr ( Expr (..), ArithmeticError (..), eval ) where

data Expr =
  Val Int |
  Expr :+: Expr |
  Expr :-: Expr |
  Expr :*: Expr |
  Expr :/: Expr |
  Expr :^: Expr deriving stock (Show, Eq)

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixr 8 :^:

data ArithmeticError = DivByZero | NegExpon deriving stock (Show, Eq)

exact :: (Int -> Int -> Int) -> Expr -> Expr -> Either ArithmeticError Int
exact op e1 e2 = do
  a <- eval e1
  b <- eval e2
  Right $ op a b

exactDiv :: Expr -> Expr -> Either ArithmeticError Int
exactDiv e1 e2 = do
  a <- eval e1
  b <- eval e2
  if b == 0 then Left DivByZero
  else Right $ div a b

exactUp :: Expr -> Expr -> Either ArithmeticError Int
exactUp e1 e2 = do
  a <- eval e1
  b <- eval e2
  if b < 0 then Left NegExpon
  else Right $ (^) a b

eval :: Expr -> Either ArithmeticError Int
eval (Val val)   = Right val
eval (e1 :+: e2) = exact (+) e1 e2
eval (e1 :-: e2) = exact (-) e1 e2
eval (e1 :*: e2) = exact (*) e1 e2
eval (e1 :/: e2) = exactDiv  e1 e2
eval (e1 :^: e2) = exactUp   e1 e2
