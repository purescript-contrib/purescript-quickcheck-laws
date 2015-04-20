module Test.QuickCheck.Laws.Data.DistributiveLattice where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

-- | - Distributivity: `x && (y || z) = (x && y) || (x && z)`
checkDistributiveLattice :: forall a. (Arbitrary a, DistributiveLattice a) => Proxy a -> QC Unit
checkDistributiveLattice _ = do

  log "Checking 'Distributivity' law for DistributiveLattice"
  quickCheck distributivity

  where

  distributivity :: a -> a -> a -> Boolean
  distributivity x y z = (x && (y || z)) == ((x && y) || (x && z))
