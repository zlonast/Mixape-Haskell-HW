{-# LANGUAGE LambdaCase #-}
module HW2.Unit3.BaseCombine where

import HW2.Unit3.CopyPaste (Parser (..))

ok :: Parser s ()
ok = pure ()

eof :: Parser s ()
eof = Parser $ \case
  [] -> Just (() ,[])
  _  -> Nothing

satisfy :: (a -> Bool) -> Parser a a
satisfy f = Parser $ \case
  []       -> Nothing
  (x : xs) -> if f x then Just (x, xs) else Nothing

element :: Eq a => a -> Parser a a
element x = satisfy (== x)

stream :: Eq a => [a] -> Parser a [a]
stream = foldr (\x -> (<*>) ((:) <$> element x)) (pure [])
