{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module HW1.Unit1.MyNat (Nat(..), isEven) where

data Nat = Z | S Nat deriving stock Show

instance Eq Nat where
  Z == Z         = True
  (S _) == Z     = False
  Z == (S _)     = False
  (S a) == (S b) = a == b

instance Ord Nat where
  Z <= Z         = True
  Z <= (S _)     = True
  (S a) <= (S b) = a <= b
  (S _) <= Z     = False

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  Z + a     = a
  a + Z     = a
  (S a) + b = S (a + b)

  (*) :: Nat -> Nat -> Nat
  Z * a           = Z
  a * Z           = Z
  (S a) * c@(S b) = (c + a) + (a * b)

  (-) :: Nat -> Nat -> Nat
  Z - _         = Z
  a - Z         = a
  (S a) - (S b) = a - b

  abs :: Nat -> Nat
  abs a = a

  signum :: Nat -> Nat
  signum _ = S Z

  fromInteger :: Integer -> Nat
  fromInteger 0 = Z
  fromInteger n | n < 0 = fromInteger (abs n)
  fromInteger n = S (fromInteger (n - 1))

instance Enum Nat where
  toEnum :: Int -> Nat
  toEnum = fromInteger . toInteger

  fromEnum :: Nat -> Int
  fromEnum Z     = 0
  fromEnum (S a) = fromEnum a + 1

instance Real Nat where
  toRational :: Nat -> Rational
  toRational Z     = 0
  toRational (S a) = toRational a + 1

instance Integral Nat where
  toInteger :: Nat -> Integer
  toInteger = toInteger . fromEnum

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem a Z = (Z, Z)
  quotRem a b | a < b = (Z, a)
              | otherwise = (S c, d) 
              where
                (c, d) = quotRem (a - b) b

class Even a where
  isEven :: a -> Bool

instance Even Nat where
  isEven :: Nat -> Bool
  isEven Z = True
  isEven (S n) = case n of
    Z   -> False
    S c -> isEven c
