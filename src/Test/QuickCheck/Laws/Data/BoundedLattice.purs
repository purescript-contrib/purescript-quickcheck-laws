module Test.QuickCheck.Laws.Data.BoundedLattice where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

-- | - Identity:
-- |   - `a || bottom = a`
-- |   - `a && top = a`
-- | - Annihiliation:
-- |   - `a || top = top`
-- |   - `a && bottom = bottom`
checkBoundedLattice :: forall a. (Arbitrary a, BoundedLattice a) => Proxy a -> QC Unit
checkBoundedLattice _ = do

  log "Checking 'Identity' law for BoundedLattice"
  quickCheck identity

  log "Checking 'Annihiliation' law for BoundedLattice"
  quickCheck annihiliation

  where

  identity :: a -> Boolean
  identity a = (a || bottom) == a
            && (a && top) == a

  annihiliation :: a -> Boolean
  annihiliation a = (a || top) == top
                 && (a && bottom) == bottom
