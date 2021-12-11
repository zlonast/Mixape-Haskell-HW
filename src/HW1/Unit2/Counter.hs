{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW1.Unit2.Counter where

newtype ADTCounter = 
  AC (forall b . (forall a . (a, a -> a, a -> a, a -> Bool) -> b) -> b)

createZero :: ADTCounter
createZero = AC (\t -> t intCounter')
{-
createZero :: ADTCounter
createZero = AC (\t -> t intCounter2')
-}
{-
createZero :: ADTCounter
createZero = AC (\t -> t doubleCounter')
-}
-- intCounter', intCounter2', doubleCounter'

inc :: ADTCounter -> ADTCounter
inc (AC r) = r x where
  x (cur, inc, dec, isZero) = AC $ \t -> t (inc cur, inc, dec, isZero) 

dec :: ADTCounter -> ADTCounter
dec (AC r) = r x where
  x (cur, inc, dec, isZero) = AC $ \t -> t (dec cur, inc, dec, isZero) 

isZero :: ADTCounter -> Bool
isZero (AC r) = r x where
  x (cur, _, _, isZero) = isZero cur

main :: IO ()
main = do
  print $ isZero $ dec $ inc createZero

----------------------------------------------------------

intCounter' :: (Int, Int -> Int, Int -> Int, Int -> Bool)
intCounter' = (createZeroInt, incInt, decInt, isZeroInt)

intCounter2' :: (Int, Int -> Int, Int -> Int, Int -> Bool)
intCounter2' = (createZeroInt, incInt2, decInt, isZeroInt)

createZeroInt :: Int
createZeroInt = 0

incInt :: Int -> Int
incInt = (1+)

incInt2 :: Int -> Int
incInt2 = (2+)

decInt :: Int -> Int
decInt = (1-)

isZeroInt :: Int -> Bool
isZeroInt = (==) 0

-----------------------------------

doubleCounter' :: (Double, Double -> Double, Double -> Double, Double -> Bool)
doubleCounter' = (createZeroDouble, incDouble, decDouble, isZeroDouble)

createZeroDouble :: Double
createZeroDouble = 0

incDouble :: Double -> Double
incDouble = succ

decDouble :: Double -> Double
decDouble = pred

isZeroDouble :: Double -> Bool
isZeroDouble = (==) 0








