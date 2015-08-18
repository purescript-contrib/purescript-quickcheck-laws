module Test.QuickCheck.Laws.Data.DivisionRing where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

import Prelude

-- | - Multiplicative inverse: `(one / x) * x = one`
checkDivisionRing :: forall a eff. (DivisionRing a, Arbitrary a, Eq a) => Proxy a -> QC eff Unit
checkDivisionRing _ = do

  log "Checking 'Multiplicative inverse' law for DivisionRing"
  quickCheck multiplicativeInverse

  where

  multiplicativeInverse :: a -> Boolean
  multiplicativeInverse x = (x == zero) || ((one / x) * x == one)
