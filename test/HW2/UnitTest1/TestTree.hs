{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeApplications   #-}
module HW2.UnitTest1.TestTree where

-- import Data.Foldable ( Foldable(fold) )
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Identity (Identity (Identity))

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW2.Unit1.Tree (Tree (..))

testsTree :: SpecWith ()
testsTree =
  describe "Task2 - Tree instance" $ do
    testFunctorTree
    testApplicativeTree
    testFoldableTree
    testTraversableTree

{-
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
-}

-- Identity: fmap id == id
-- Composition: fmap (f . g) == fmap f . fmap g

testFunctorTree :: SpecWith ()
testFunctorTree = it "testFunctorTree" $ do
  fmap (toInteger @Int) (Leaf 5) `shouldBe` Leaf (5 :: Integer)
  fmap id (Leaf (5 :: Integer))  `shouldBe` Leaf (5 :: Integer)
  fmap (fromInteger @Int . toInteger @Int) (Leaf (5 :: Int)) `shouldBe` (fmap fromInteger . fmap (toInteger @Int)) (Leaf (5 :: Int))

-- Identity: `pure`   `id`   `<*>`  v = v
-- Composition: `pure`  (.)  `<*>`  u  `<*>`  v  `<*>`  w = u  `<*>`  (v  `<*>`  w)
-- Homomorphism: `pure`  f  `<*>`   `pure`  x =  `pure`  (f x)
-- Interchange: u  `<*>`   `pure`  y =  `pure`  ( `$`  y)  `<*>`  u

testApplicativeTree :: SpecWith ()
testApplicativeTree = it "testApplicativeTree" $ do
  pure 5 `shouldBe` Leaf (5 :: Integer)
  pure id <*> w                 `shouldBe` w
  pure  (.)  <*> u <*> v <*>  w `shouldBe` u <*> (v  <*>  w)
  pure @Tree f <*> pure x       `shouldBe` pure (f x)
  pure @Tree f <*> pure w       `shouldBe` pure (f w)
  u <*> pure y                  `shouldBe` pure ($ y) <*> u
    where
      u = Leaf id
      v = Leaf (const @Integer 3)
      w = Leaf (8 :: Integer)
      y = 5 :: Integer
      x = 3 :: Integer
      f = id

{-
Foldable instances are expected to satisfy the following laws:

foldr f z t = appEndo (foldMap (Endo . f) t ) z
foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
fold = foldMap id
length = getSum . foldMap (Sum . const  1)
sum , product , maximum , and minimum should all be essentially equivalent to foldMap forms, such as

sum = getSum . foldMap Sum
but may be less defined.
If the type is also a Functor instance, it should satisfy

foldMap f = fold . fmap f
which implies that

foldMap f . fmap g = foldMap (f . g)

-}

-- foldr f (Leaf (5 :: Integer)) t = appEndo (foldMap (Endo . f) t ) z

-- fold :: (Foldable t, Monoid m) => t m -> m

testFoldableTree :: SpecWith ()
testFoldableTree = it "testFoldableTree" $ do
  foldr f z t `shouldBe` unEndo (foldMap (Endo . f) t ) z
  foldl f z t `shouldBe` unEndo (unDual (foldMap (Dual . Endo . flip f) t)) z
  -- fold `shouldBe` foldMap id
  length t `shouldBe` (unSum . foldMap (Sum . const  1)) t
  sum t `shouldBe` (unSum . foldMap Sum) t
  product t `shouldBe` (unProduct . foldMap Product) t
  -- foldMap f `shouldBe` fold . fmap f
  (foldMap f2 . fmap g) t `shouldBe` foldMap (f2 . g) t
    where
      f = (+)
      g = id
      f2 = Sum
      z = 0 :: Int
      t = Leaf (5 :: Int)

{-
A definition of traverse must satisfy the following laws:

Naturality: t . traverse f = traverse (t . f) for every applicative transformation t
Identity: traverse Identity = Identity
Composition: traverse ( Data.Functor.Compose.Compose . fmap g . f) = Data.Functor.Compose.Compose . fmap ( traverse g) . traverse f
A definition of sequenceA must satisfy the following laws:

Naturality: t . sequenceA = sequenceA . fmap t for every applicative transformation t
Identity: sequenceA . fmap Identity = Identity
Composition: sequenceA . fmap Data.Functor.Compose.Compose = Data.Functor.Compose.Compose . fmap sequenceA . sequenceA

A result of the naturality law is a purity law for traverse

`traverse`   `pure`  =  `pure`
-}

{-
newtype Identity a = Identity {unIdentity :: a} deriving stock (Show, Eq)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure :: a -> Identity a
  pure a = Identity a

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) (Identity f) (Identity a) = Identity $ f a
-}

testTraversableTree :: SpecWith ()
testTraversableTree = it "testTraversableTree" $ do
  -- (k . traverse f) t `shouldBe` (traverse (k . f)) t
  traverse (Identity @Integer) t `shouldBe` Identity t
  traverse (Compose . fmap g . f) t `shouldBe` (Compose . fmap ( traverse g) . traverse f) t
  -- t . sequenceA = sequenceA . fmap t
  (sequenceA . fmap Identity) t `shouldBe` Identity t
  (sequenceA . fmap Compose) t2 `shouldBe` (Compose . fmap sequenceA . sequenceA) t2
  traverse pure t `shouldBe` pure @Tree t
    where
      t = Leaf (5 :: Integer)
      t2 = Identity $ Identity $ Identity (5 :: Integer)
      f = Identity @Integer
      g = Identity @Integer
      -- k = runIdentity



















newtype Sum = Sum {unSum :: Int} deriving stock (Show, Eq)

instance Semigroup Sum where (Sum a) <> (Sum b) = Sum (a + b)

instance Monoid Sum where mempty = Sum 0

newtype Product = Product {unProduct :: Int} deriving stock (Show, Eq)

instance Semigroup Product where
    (Product a) <> (Product b) = Product (a * b)

instance Monoid Product where
    mempty = Product 1

newtype Name = Name { unName :: String }  deriving stock (Eq, Show)

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (Name a) <> (Name b) = Name $ a ++ "." ++ b

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a = Endo { unEndo :: a -> a }

{-
instance Semigroup (Endo a) where (Endo a) <> (Endo b) = Endo $ a . b

instance Monoid (Endo a) where mempty = Endo id
-}

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (Endo a) <> (Endo b) = Endo $ a . b

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id

newtype Dual t a = Dual { unDual :: t a}

instance (Semigroup (t a)) => Semigroup (Dual t a) where
  (<>) :: Dual t a -> Dual t a -> Dual t a
  (Dual m1) <> (Dual m2) = Dual $ m1 <> m2

instance (Monoid (t a)) => Monoid (Dual t a) where
  mempty :: Dual t a
  mempty = Dual mempty











