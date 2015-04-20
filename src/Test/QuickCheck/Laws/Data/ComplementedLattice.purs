module Test.QuickCheck.Laws.Data.ComplementedLattice where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

-- | - Complemented:
-- |   - `not a || a == top`
-- |   - `not a && a == bottom`
-- | - Double negation:
-- |   - `not <<< not == id`
checkComplementedLattice :: forall a. (Arbitrary a, ComplementedLattice a) => Proxy a -> QC Unit
checkComplementedLattice _ = do

  log "Checking 'Complemented' law for ComplementedLattice"
  quickCheck complemented

  log "Checking 'Double negation' law for ComplementedLattice"
  quickCheck doubleNegation

  where

  complemented :: a -> Boolean
  complemented a = (not a || a) == top
                && (not a && a) == bottom

  doubleNegation :: a -> Boolean
  doubleNegation a = (not <<< not) a == id a
