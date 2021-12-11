module HW2.Unit1.StringSum (stringSum) where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum str = sum <$> traverse readMaybe (filter (/="") . splitOn " " $ str)
