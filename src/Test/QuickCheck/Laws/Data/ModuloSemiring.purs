module Test.QuickCheck.Laws.Data.ModuloSemiring where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)
import Type.Proxy (Proxy())

-- | - Remainder: ```a / b * b + (a `mod` b) = a```
checkModuloSemiring :: forall a. (ModuloSemiring a, Arbitrary a, Eq a) => Proxy a -> QC Unit
checkModuloSemiring _ = do

  trace "Checking 'Remainder' law for ModuloSemiring"
  quickCheck remainder

  where

  remainder :: a -> a -> Boolean
  remainder a b = a / b * b + (a `mod` b) == a
