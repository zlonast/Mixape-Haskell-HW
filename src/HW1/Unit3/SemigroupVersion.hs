{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module HW1.Unit3.SemigroupVersion (
  NonEmpty (..), ThisOrThat (..), Name(..), Endo(..)) where

data NonEmpty a = a :| [a] deriving stock (Eq, Show)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (a :| as) <> (b :| bs) = a :| (as ++ (b : bs))

data ThisOrThat a b = This a | That b | Both a b deriving stock (Eq, Show)

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (This a)    <> (This b)     = This a
  (That a)    <> (That b)     = That a
  (This a)    <> (That b)     = Both a b
  (That b)    <> (This a)     = Both a b
  (This a)    <> (Both a0 b)  = Both a b
  (That b)    <> (Both a b0)  = Both a b
  (Both a0 b) <> (This a)     = Both a b
  (Both a b0) <> (That b)     = Both a b
  (Both a b)  <> (Both a0 b0) = Both a b

newtype Name = Name { unName :: String }  deriving stock (Eq, Show)

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (Name a) <> (Name b) = Name $ a ++ "." ++ b

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a = Endo { unEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (Endo a) <> (Endo b) = Endo $ a . b

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
