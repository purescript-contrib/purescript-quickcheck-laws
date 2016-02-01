module Test.QuickCheck.Laws.Data.DivisionRing where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (Arbitrary)

-- | - Multiplicative inverse: `(one / x) * x = one`
checkDivisionRing :: forall eff a. (DivisionRing a, Arbitrary a, Eq a) => Proxy a -> QC eff Unit
checkDivisionRing _ = do

  log "Checking 'Multiplicative inverse' law for DivisionRing"
  quickCheck' 1000 multiplicativeInverse

  where

  multiplicativeInverse :: a -> Boolean
  multiplicativeInverse x = (x == zero) || ((one / x) * x == one)
