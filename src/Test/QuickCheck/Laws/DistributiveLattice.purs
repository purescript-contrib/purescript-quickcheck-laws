module Test.QuickCheck.Laws.DistributiveLattice where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Distributivity: `x && (y || z) = (x && y) || (x && z)`
checkDistributiveLattice :: forall m a. (Arbitrary a, DistributiveLattice a) => a -> QC Unit
checkDistributiveLattice _ = do

  trace "Checking 'Distributivity' law for DistributiveLattice"
  quickCheck distributivity

  where

  distributivity :: a -> a -> a -> Boolean
  distributivity x y z = (x && (y || z)) == ((x && y) || (x && z))
