module Test.QuickCheck.Laws.Ring where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

checkRing :: forall a. (Ring a, Arbitrary a, Eq a) => a -> QC Unit
checkRing _ = do

  trace "Checking 'Additive inverse' law for Ring"
  quickCheck additiveInverse

  where

  additiveInverse :: a -> Boolean
  additiveInverse a = a + (-a) == zero
