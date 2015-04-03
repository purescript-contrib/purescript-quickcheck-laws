module Test.QuickCheck.Laws.Bounded where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Ordering: `bottom <= a <= top`
checkBounded :: forall a. (Arbitrary a, Bounded a) => a -> QC Unit
checkBounded _ = do

  trace "Checking 'Ordering' law for Bounded"
  quickCheck ordering

  where

  ordering :: a -> Boolean
  ordering a = bottom <= a && a <= top
