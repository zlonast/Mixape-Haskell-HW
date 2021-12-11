{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW1.Unit2.Split (splitOn, joinWith) where

import qualified Data.List.NonEmpty as NL

splitOn :: forall a . Eq a => a -> [a] -> NL.NonEmpty [a]
splitOn c = foldr split ([] NL.:| []) 
  where
    split :: a -> NL.NonEmpty [a] -> NL.NonEmpty [a]
    split a (b NL.:| bs) | a == c    = [] NL.:| (b : bs)
                         | otherwise = (a : b) NL.:| bs

joinWith :: a -> NL.NonEmpty [a] -> [a]
joinWith c = foldl1 (\l r -> l ++ [c] ++ r)
