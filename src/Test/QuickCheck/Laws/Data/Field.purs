module Test.QuickCheck.Laws.Data.Field where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Multiplicative inverse: `(one / x) * x = one`
checkField
  :: forall eff a. (Field a, Arbitrary a, Eq a) => Proxy a -> QC eff Unit
checkField _ = do

  log "Checking 'Multiplicative inverse' law for Field"
  quickCheck' 1000 multiplicativeInverse

  where

  multiplicativeInverse :: a -> Boolean
  multiplicativeInverse x = (x == zero) || ((one / x) * x == one)
