{-# LANGUAGE BlockArguments #-}
module HW0.T4 where

import           Data.Function   (fix)
import           Numeric.Natural (Natural)

-- fix (\rec n -> if n <= 1 then 1 else n * rec (n-1)) 5

repeat' :: a -> [a]             -- behaves like Data.List.repeat
-- repeat' = fix (\rec x -> x : rec x) не понимаю почему тесты не проходит :(
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
map' = fix (\rec f ls -> case ls of [] -> []; (a : as) -> f a : rec f as)

fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib n = (0 : fix (scanl (+) 1 . (0 :))) !! fromInteger (toInteger n)
-- fib n = (fix (\fib -> scanl (+) 1 (0 : fib))) !! (fromInteger $ toInteger n)
{-
scanl (+) 1 (0:fibs)
1:scanl (+) 1 (0:1:drop 1 fibs)
1:1:scanl (+) 1 (0:1:1:drop 3 fibs)
1:1:2:scanl (+) (0:1:1:2:drop 3 fibs)
-}
--fib =
--  fix (\rec n ->
--     if n == 0 then 0 else if n <= 1 then 1 else rec (n - 2) + rec (n - 1))

fac :: Natural -> Natural       -- computes the factorial
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))
















