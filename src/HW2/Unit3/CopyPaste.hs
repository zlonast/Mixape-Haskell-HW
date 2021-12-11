{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module HW2.Unit3.CopyPaste (Parser(..)) where

import Control.Applicative (Alternative (empty, (<|>)))

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser $ \ls -> case p ls of
    Nothing     -> Nothing
    Just (a, b) -> Just (f a, b)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \ls -> Just (a, ls)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) pf pa = do { f <- pf; f <$> pa; }

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser pa) f = Parser $ \ s0 -> case pa s0 of
    Nothing      -> Nothing
    Just (a, s1) -> case f a of { Parser g -> g s1 }

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser pa) (Parser pb) = Parser $ \ s0 -> pa s0 <|> pb s0
