module Test.QuickCheck.Laws.DivisionRing where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

checkDivisionRing :: forall a. (DivisionRing a, Arbitrary a, Eq a) => a -> QC Unit
checkDivisionRing _ = do

  trace "Checking 'Multiplicative inverse' law for DivisionRing"
  quickCheck multiplicativeInverse

  where

  multiplicativeInverse :: a -> Boolean
  multiplicativeInverse x = (x == zero) || ((one / x) * x == one)
