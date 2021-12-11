{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module HW2.Unit1.Tree (Tree(..)) where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a deriving stock (Eq, Show)

-- Identity: fmap id == id
-- Composition: fmap (f . g) == fmap f . fmap g
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)       = Leaf (f a)
  fmap f (Branch tl tr) = Branch (fmap f tl) (fmap f tr)

-- Identity: `pure`   `id`   `<*>`  v = v
-- Composition: `pure`  (.)  `<*>`  u  `<*>`  v  `<*>`  w = u  `<*>`  (v  `<*>`  w)
-- Homomorphism: `pure`  f  `<*>`   `pure`  x =  `pure`  (f x)
-- Interchange: u  `<*>`   `pure`  y =  `pure`  ( `$`  y)  `<*>`  u
instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) (Leaf a)   = Leaf (f a)
  (<*>) (Leaf f) tree       = fmap f tree
  (<*>) (Branch fl fr) tree = Branch (fl <*> tree) (fr <*> tree)

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
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf x)     = f x z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

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

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r


