module Test.QuickCheck.Laws.Data.Ring where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Additive inverse: `a - a = a + (-a) = (-a) + a = zero`
checkRing
  ∷ ∀ a
  . Ring a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkRing _ = checkRingGen (arbitrary :: Gen a)

checkRingGen
  ∷ ∀ a
  . Ring a
  ⇒ Eq a
  ⇒ Gen a
  → Effect Unit
checkRingGen gen = do
  log "Checking 'Additive inverse' law for Ring"
  quickCheck' 1000 $ additiveInverse <$> gen

  where

  additiveInverse ∷ a → Boolean
  additiveInverse a = a - a == zero && a + (-a) == zero && (-a) + a == zero
