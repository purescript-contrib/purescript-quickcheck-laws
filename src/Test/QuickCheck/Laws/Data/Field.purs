module Test.QuickCheck.Laws.Data.Field where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Non-zero multiplicative inverse: ``a `mod` b = 0` for all `a` and `b`
checkField
  ∷ ∀ eff a
  . Field a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkField _ = do

  log "Checking 'Non-zero multiplicative inverse' law for Field"
  quickCheck' 1000 multiplicativeInverse

  where

  multiplicativeInverse ∷ a → a → Boolean
  multiplicativeInverse x y = x `mod` y == zero
