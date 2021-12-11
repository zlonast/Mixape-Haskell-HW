{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW1.Unit3.Concat (maybeConcat, eitherConcat) where

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldl concatMy [] 
  where
    concatMy :: [a] -> Maybe [a] -> [a]
    concatMy b (Just a) = b ++ a
    concatMy b Nothing  = b

eitherConcat :: forall a b . (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldl concatMy (mempty, mempty) 
  where
    concatMy :: (a, b) -> Either a b -> (a, b)
    concatMy (a, b) (Left c)  = (mappend a c, b)
    concatMy (a, b) (Right c) = (a, mappend b c)
