{-# LANGUAGE TypeApplications #-}
module HW2.UnitTest3.TestCopyPaste where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW2.Unit3.CopyPaste (Parser(..))

testsCopyPaste :: SpecWith ()
testsCopyPaste =
  describe "Task6 - copy and paste" $ do
    testFunctorParser
    testApplicativeParser
    testMonadParser
    --testAlternativeParser

-- Identity: fmap id == id
-- Composition: fmap (f . g) == fmap f . fmap g

testFunctorParser :: SpecWith ()
testFunctorParser = it "testFunctorParser" $ do
  runParser (fmap (toInteger @Int) p0) "abc" `shouldBe` runParser p1 "abc"
  runParser (fmap id p1) "abc" `shouldBe` runParser p1 "abc"
  runParser (fmap (fromInteger @Int . toInteger @Int) p0) "abc" `shouldBe` runParser ((fmap fromInteger . fmap (toInteger @Int)) p0) "abc"
    where
      p0 = Parser $ \ls -> Just (1 :: Int, ls)
      p1 = Parser $ \ls -> Just (1 :: Integer, ls)

-- Identity: `pure`   `id`   `<*>`  v = v
-- Composition: `pure`  (.)  `<*>`  u  `<*>`  v  `<*>`  w = u  `<*>`  (v  `<*>`  w)
-- Homomorphism: `pure`  f  `<*>`   `pure`  x =  `pure`  (f x)
-- Interchange: u  `<*>`   `pure`  y =  `pure`  ( `$`  y)  `<*>`  u

testApplicativeParser :: SpecWith ()
testApplicativeParser = it "testApplicativeParser" $ do
  runParser (pure 5) "abc"                     `shouldBe` runParser p "abc"
  runParser (pure id <*> w) "abc"              `shouldBe` runParser w "abc"
  runParser (pure (.) <*> u <*> v <*> w) "abc" `shouldBe` runParser (u <*> (v  <*>  w)) "abc"
  runParser (pure f <*> pure x) "abc"          `shouldBe` runParser (pure (f x)) "abc"
  -- runParser (pure f <*> pure w) "abc"             `shouldBe` runParser (pure (f w)) "abc"
  runParser (u <*> pure y) "abc"               `shouldBe` runParser (pure ($ y) <*> u) "abc"
    where
      p = Parser $ \ls -> Just (5 :: Integer, ls)
      u = Parser $ \ls -> Just (id, ls)
      v = Parser $ \ls -> Just (const @Integer 3, ls)
      w = Parser $ \ls -> Just (8 :: Integer, ls)
      y = 5 :: Integer
      x = 3 :: Integer
      f = id


{-
Instances of Monad should satisfy the following:

Left identity: return a >>= k = k a
Right identity: m >>= return = m
Associativity: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
Furthermore, the Monad and Applicative operations should relate as follows:

`pure`  =  `return`
m1  `<*>`  m2 = m1  `>>=`  (x1 -> m2  `>>=`  (x2 ->  `return`  (x1 x2)))
The above laws imply:

`fmap`  f xs  =  xs  `>>=`   `return`  . f
( `>>` ) = ( `*>` )

and that pure and ( <*> ) satisfy the applicative functor laws.

The instances of Monad for lists, Data.Maybe.Maybe and System.IO.IO defined in the Prelude satisfy these laws.
-}

testMonadParser :: SpecWith ()
testMonadParser = it "testMonadParser" $ do
  runParser (return a >>= k) "abc"          `shouldBe` runParser (k a) "abc"
  runParser (m >>= return) "abc"            `shouldBe` runParser m "abc"
  runParser (m >>= (\x -> k x >>= h)) "abc" `shouldBe` runParser ((m >>= k) >>= h) "abc"
  runParser (pure a) "abc"                  `shouldBe` runParser (return a) "abc"
  runParser (m1 <*> m2) "abc"               `shouldBe` runParser (m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))) "abc"
  runParser (fmap f xs) "abc"               `shouldBe` runParser (xs >>= return . f) "abc"
  runParser (m >> m2) "abc"                 `shouldBe` runParser (m *> m2) "abc"
    where
      -- p = Parser $ \ls -> Just (5 :: Integer, ls)
      f = \_ -> 3 :: Int
      xs = Parser $ \ls -> Just (3 :: Int, ls)
      m1 = Parser $ \ls -> Just ((\_ -> 3 :: Int), ls)
      m2 = Parser $ \ls -> Just (3 :: Int, ls)
      a = 5 :: Int
      m = Parser $ \ls -> Just (3 :: Int, ls)
      h = \_ -> Parser $ \ls -> Just (4 :: Int, ls)
      k = \_ -> Parser $ \ls -> Just (3 :: Int, ls)































