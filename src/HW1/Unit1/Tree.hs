{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DerivingStrategies #-}
module HW1.Unit1.Tree (
    Tree(..), isEmpty, size, search, insert, fromList, erase) where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NL

data Tree a = Leaf | Node (Tree a) (NL.NonEmpty a) (Tree a) deriving stock Show

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf         = 0
size (Node a _ b) = size a + size b + 1

search :: Ord a => a -> Tree a -> Bool
search _ Leaf = False
search a (Node l (b NL.:| bs) r)
  | a < b     = search a l
  | a > b     = search a r
  | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node Leaf (a NL.:| []) Leaf
insert a (Node l list@(b NL.:| bs) r)
  | a < b     = Node (insert a l) list r
  | a > b     = Node l list (insert a r)
  | otherwise = Node l (a NL.:| (b : bs)) r

fromList :: Ord a => [a] -> Tree a
fromList ls = build sorted
  where
    sorted = L.group . L.sort $ ls
    len    = length ls
    build :: [[a]] -> Tree a
    build []       = Leaf
    build (a : ls) = mid a Leaf Leaf ls (-1)
    left :: [[a]] -> Tree a -> Int -> Tree a
    left (a : ls) tree s
      | 2 * s < len = mid a tree (build (take s ls)) (drop s ls) s
      | otherwise   = Node tree (NL.fromList a) (build ls)
    left [] t _ = t
    mid :: [a] -> Tree a -> Tree a -> [[a]] -> Int -> Tree a
    mid a t1 t2 [] _ = Node t1 (NL.fromList a) t2
    mid a t1 t2 ls s = left ls (Node t1 (NL.fromList a) t2) (s + 2)

erase :: Ord a => a -> Tree a -> Tree a
erase _ Leaf = Leaf
erase a n@(Node l list@(b NL.:| bs) r)
  | a < b     = Node (erase a l) list r
  | a > b     = Node l list (erase a r)
  | otherwise = delete n
  where
    delete :: Tree a -> Tree a
    delete Leaf                          = Leaf
    delete (Node l (_ NL.:| []) r)       = build l r
    delete (Node l (_ NL.:| (b : bs)) r) = Node l (b NL.:| bs) r

    build :: Tree a -> Tree a -> Tree a
    build Leaf right = right
    build left Leaf  = left
    build (Node l nl r) right = newTree
      where
        (list, newLeft) = goRight l nl r
        newTree = Node newLeft list right

    goRight :: Tree a -> NL.NonEmpty a -> Tree a -> (NL.NonEmpty a, Tree a)
    goRight left list Leaf = (list, left)
    goRight left list (Node l nl r) = (newList, Node left list newRight)
      where
        (newList, newRight) = goRight l nl r