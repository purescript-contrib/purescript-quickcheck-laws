module Test.QuickCheck.Laws.Data.DivisionRing where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

-- | - Multiplicative inverse: `(one / x) * x = one`
checkDivisionRing :: forall a. (DivisionRing a, Arbitrary a, Eq a) => Proxy a -> QC Unit
checkDivisionRing _ = do

  log "Checking 'Multiplicative inverse' law for DivisionRing"
  quickCheck multiplicativeInverse

  where

  multiplicativeInverse :: a -> Boolean
  multiplicativeInverse x = (x == zero) || ((one / x) * x == one)
