module Test.QuickCheck.Laws.BoundedLattice where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Identity:
-- |   - `a || bottom = a`
-- |   - `a && top = a`
-- | - Annihiliation:
-- |   - `a || top = top`
-- |   - `a && bottom = bottom`
checkBoundedLattice :: forall a. (Arbitrary a, BoundedLattice a) => a -> QC Unit
checkBoundedLattice _ = do

  trace "Checking 'Identity' law for BoundedLattice"
  quickCheck identity

  trace "Checking 'Annihiliation' law for BoundedLattice"
  quickCheck annihiliation

  where

  identity :: a -> Boolean
  identity a = (a || bottom) == a
            && (a && top) == a

  annihiliation :: a -> Boolean
  annihiliation a = (a || top) == top
                 && (a && bottom) == bottom
