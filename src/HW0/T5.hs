module HW0.T5 where

import           Data.Function   (fix)
import           Numeric.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns n f x = n f (f x)

nplus :: Nat a -> Nat a -> Nat a
nplus a b f x = a f (b f x)

nmult :: Nat a -> Nat a -> Nat a
nmult a b f = a (b f)

nFromNatural :: Natural -> Nat a
nFromNatural = fix (\rec n f x -> if n > 0 then rec (n - 1) f (f x) else x)

nToNum :: Num a => Nat a -> a
nToNum nat = nat (+1) 0


--nToNum nz       ==  0
--nToNum (ns x)   ==  1 + nToNum x

--nToNum (nplus a b)   ==   nToNum a + nToNum b
--nToNum (nmult a b)   ==   nToNum a * nToNum b






















