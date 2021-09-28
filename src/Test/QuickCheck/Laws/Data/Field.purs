module Test.QuickCheck.Laws.Data.Field where

import Prelude

import Control.Apply (lift2)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Non-zero multiplicative inverse: ```a `mod` b = 0``` for all `a` and `b`
checkField
  ∷ ∀ a
  . Field a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkField _ = checkFieldGen (arbitrary :: Gen a)

checkFieldGen
  ∷ ∀ a
  . Field a
  ⇒ Eq a
  ⇒ Gen a
  → Effect Unit
checkFieldGen gen = do
  log "Checking 'Non-zero multiplicative inverse' law for Field"
  quickCheck' 1000 $ lift2 multiplicativeInverse gen gen

  where

  multiplicativeInverse ∷ a → a → Boolean
  multiplicativeInverse x y = x `mod` y == zero
