{-# LANGUAGE InstanceSigs #-}
module HW2.Bonus.Cont where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont s) where
  fmap :: (a -> b) -> Cont s a -> Cont s b
  fmap f (Cont arr) = Cont $ \bs -> arr $ \a -> bs (f a)

instance Applicative (Cont s) where
  pure :: a -> Cont s a
  pure a = Cont ($ a)
  (<*>) :: Cont s (a -> b) -> Cont s a -> Cont s b
  (<*>) pf pa = do { f <- pf; f <$> pa; }

instance Monad (Cont s) where
  return :: a -> Cont s a
  return a = Cont ($ a)

  (>>=) :: Cont s a -> (a -> Cont s b) -> Cont s b
  (Cont arr) >>= f = Cont $ \br -> arr $ \a -> runCont (f a) br

withOs :: (String -> r) -> r 
withOs f = f "Hello"





