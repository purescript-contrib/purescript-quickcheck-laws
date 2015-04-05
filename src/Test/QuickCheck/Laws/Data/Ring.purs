module Test.QuickCheck.Laws.Data.Ring where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)
import Type.Proxy (Proxy())

-- | - Additive inverse: `a + (-a) = (-a) + a = zero`
checkRing :: forall a. (Ring a, Arbitrary a, Eq a) => Proxy a -> QC Unit
checkRing _ = do

  trace "Checking 'Additive inverse' law for Ring"
  quickCheck additiveInverse

  where

  additiveInverse :: a -> Boolean
  additiveInverse a = a + (-a) == zero
