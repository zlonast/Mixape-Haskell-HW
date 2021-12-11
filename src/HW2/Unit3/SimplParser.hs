module HW2.Unit3.SimplParser where

import Control.Applicative (some, (<|>))
import Data.Char (isDigit)

import HW2.Unit3.BaseCombine (element, eof, satisfy)
import HW2.Unit3.CopyPaste (Parser (..))

isCorrectBrackets :: Parser Char ()
isCorrectBrackets = parse >> eof
  where
    parse :: Parser Char ()
    parse = (element '(' >> parse >> element ')' >> parse) <|> pure ()

readNumber :: Parser Char Int
readNumber = pred <*> num
  where
    pred :: Parser Char (Int -> Int)
    pred = (element '+' >> pure id) <|> (element '-' >> pure negate) <|> pure id
    num :: Parser Char Int
    num = read <$> some (satisfy isDigit)










