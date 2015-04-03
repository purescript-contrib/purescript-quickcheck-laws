module Test.QuickCheck.Laws.ModuloSemiring where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Remainder: ```a / b * b + (a `mod` b) = a```
checkModuloSemiring :: forall a. (ModuloSemiring a, Arbitrary a, Eq a) => a -> QC Unit
checkModuloSemiring _ = do

  trace "Checking 'Remainder' law for ModuloSemiring"
  quickCheck remainder

  where

  remainder :: a -> a -> Boolean
  remainder a b = a / b * b + (a `mod` b) == a
