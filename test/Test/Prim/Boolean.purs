module Test.Prim.Boolean where

import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.BoundedLattice
import Test.QuickCheck.Laws.Data.ComplementedLattice
import Test.QuickCheck.Laws.Data.DistributiveLattice
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Lattice
import Test.QuickCheck.Laws.Data.Ord
import Type.Proxy (Proxy(..))

prxBoolean :: Proxy Boolean
prxBoolean = Proxy

checkBoolean = do
  checkEq prxBoolean
  checkOrd prxBoolean
  checkBounded prxBoolean
  checkLattice prxBoolean
  checkBoundedLattice prxBoolean
  checkComplementedLattice prxBoolean
  checkDistributiveLattice prxBoolean
