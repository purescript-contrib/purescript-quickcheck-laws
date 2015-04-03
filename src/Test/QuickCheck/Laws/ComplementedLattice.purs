module Test.QuickCheck.Laws.ComplementedLattice where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Complemented:
-- |   - `not a || a == top`
-- |   - `not a && a == bottom`
-- | - Double negation:
-- |   - `not <<< not == id`
checkComplementedLattice :: forall a. (Arbitrary a, ComplementedLattice a) => a -> QC Unit
checkComplementedLattice _ = do

  trace "Checking 'Complemented' law for ComplementedLattice"
  quickCheck complemented

  trace "Checking 'Double negation' law for ComplementedLattice"
  quickCheck doubleNegation

  where

  complemented :: a -> Boolean
  complemented a = (not a || a) == top
                && (not a && a) == bottom

  doubleNegation :: a -> Boolean
  doubleNegation a = (not <<< not) a == id a
