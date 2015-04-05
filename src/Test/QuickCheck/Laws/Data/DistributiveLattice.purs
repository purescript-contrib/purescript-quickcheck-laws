module Test.QuickCheck.Laws.Data.DistributiveLattice where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)
import Type.Proxy (Proxy())

-- | - Distributivity: `x && (y || z) = (x && y) || (x && z)`
checkDistributiveLattice :: forall a. (Arbitrary a, DistributiveLattice a) => Proxy a -> QC Unit
checkDistributiveLattice _ = do

  trace "Checking 'Distributivity' law for DistributiveLattice"
  quickCheck distributivity

  where

  distributivity :: a -> a -> a -> Boolean
  distributivity x y z = (x && (y || z)) == ((x && y) || (x && z))
