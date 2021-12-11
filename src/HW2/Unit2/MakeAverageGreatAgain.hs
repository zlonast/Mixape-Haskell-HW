{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW2.Unit2.MakeAverageGreatAgain (moving) where

import Control.Monad (forM)
import Control.Monad.Trans.State (StateT, evalState, get, modify)
import Data.Functor.Identity (Identity)

moving :: forall a . Fractional a => Int -> [a] -> [a]
moving n xs = evalState (forM xs step) []
  where
    average :: [a] -> a
    average xs = sum xs / fromIntegral (length xs)
    step :: a -> StateT [a] Identity a
    step x = modify ((x:) . take (n-1)) >> average <$> get




