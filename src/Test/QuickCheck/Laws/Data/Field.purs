module Test.QuickCheck.Laws.Data.Field where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Non-zero multiplicative inverse: ``a `mod` b = 0` for all `a` and `b`
checkField
  ∷ ∀ eff a
  . Field a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkField _ = checkFieldGen (arbitrary :: Gen a)

checkFieldGen
  ∷ ∀ eff a
  . Field a
  ⇒ Eq a
  ⇒ Gen a
  → QC eff Unit
checkFieldGen gen = do

  log "Checking 'Non-zero multiplicative inverse' law for Field"
  quickCheck' 1000 $ lift2 multiplicativeInverse gen gen

  where

  multiplicativeInverse ∷ a → a → Boolean
  multiplicativeInverse x y = x `mod` y == zero
