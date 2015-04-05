module Test.QuickCheck.Laws.Data.Bounded where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)
import Type.Proxy (Proxy())

-- | - Ordering: `bottom <= a <= top`
checkBounded :: forall a. (Arbitrary a, Bounded a) => Proxy a -> QC Unit
checkBounded _ = do

  trace "Checking 'Ordering' law for Bounded"
  quickCheck ordering

  where

  ordering :: a -> Boolean
  ordering a = bottom <= a && a <= top
