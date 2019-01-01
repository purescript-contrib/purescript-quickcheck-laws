module Test.QuickCheck.Laws.Data.Ring where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Combinators ((&=&))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

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


-- | Like `checkRing`, but with better error reporting.
-- | - Additive inverse: `a - a = a + (-a) = (-a) + a = zero`
checkRingShow
  ∷ ∀ a
  . Ring a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → Effect Unit
checkRingShow _ = do

  log "Checking 'Additive inverse' law for Ring"
  quickCheck' 1000 additiveInverse

  where

  additiveInverse ∷ a → Result
  additiveInverse a = (a - a === zero) &=& (a + (-a) === zero) &=& ((-a) + a === zero)
