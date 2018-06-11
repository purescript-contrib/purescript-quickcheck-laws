module Test.QuickCheck.Laws.Data.Ring where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Type.Proxy (Proxy)

import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Additive inverse: `a - a = a + (-a) = (-a) + a = zero`
checkRing
  ∷ ∀ a
  . Ring a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkRing _ = do

  log "Checking 'Additive inverse' law for Ring"
  quickCheck' 1000 additiveInverse

  where

  additiveInverse ∷ a → Boolean
  additiveInverse a = a - a == zero && a + (-a) == zero && (-a) + a == zero
