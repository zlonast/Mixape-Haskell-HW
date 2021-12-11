{-# LANGUAGE InstanceSigs #-}
module HW1.Unit2.FoldTree (Tree(..), toList) where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NL

import HW1.Unit1.Tree (Tree (..))

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node left list right) =
    mconcat (NL.toList (NL.map f list)) `mappend` 
    foldMap f left `mappend` foldMap f right

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z Leaf            = z
  foldr f z (Node l list r) = foldr f (foldr f (foldr f z r) list) l
