module HW2.Unit3.HardParser where

import Control.Applicative (many, (<|>))
import Data.Char (isSpace)

import HW2.Unit3.BaseCombine (element, eof, satisfy)
import HW2.Unit3.CopyPaste (Parser (..))
import HW2.Unit3.SimplParser (readNumber)

listlistParser :: Parser Char [[Int]]
listlistParser = (do
      space
      many (element ',')
      space
      list <- readNumber >>= parseList
      res  <- listlistParser
      return (list : res)) <|> (eof >> return []) 
  where
    parseList :: Int -> Parser Char [Int]
    parseList n = do
      comma
      num <- readNumber
      if n > 1 then do
        list <- parseList (n - 1)
        return (num : list)
      else
        return [num]
    
    comma :: Parser Char [Char]
    comma = space >> element ',' >> space
    
    space :: Parser Char [Char]
    space = many (satisfy isSpace)





















