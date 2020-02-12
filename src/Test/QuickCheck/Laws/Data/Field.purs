module Test.QuickCheck.Laws.Data.Field where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

-- | - Non-zero multiplicative inverse: ```a `mod` b = 0``` for all `a` and `b`
checkField
  ∷ ∀ a
  . Field a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkField _ = do

  log "Checking 'Non-zero multiplicative inverse' law for Field"
  quickCheck' 1000 multiplicativeInverse

  where

  multiplicativeInverse ∷ a → a → Boolean
  multiplicativeInverse x y = x `mod` y == zero
