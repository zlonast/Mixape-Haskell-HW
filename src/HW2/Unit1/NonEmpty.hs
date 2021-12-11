{-# LANGUAGE InstanceSigs #-}
module HW2.Unit1.NonEmpty (NonEmpty(..)) where

data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (a :| as) = f a :| fmap f as

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| []
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) m1 m2 = do { x1 <- m1; x1 <$> m2; }

instance Monad NonEmpty where
  (>>=)  :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (a :| as) >>= f = b :| (bs ++ bs')
    where
      b :| bs = f a
      bs' = as >>= toList . f
      toList (c :| cs) = c : cs

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (a :| as) = f a `mappend` foldMap f as

-- (<$>) :: Functor f => (a -> b) -> f a -> f b

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (a :| as) = (:|) <$> f a <*> traverse f as







