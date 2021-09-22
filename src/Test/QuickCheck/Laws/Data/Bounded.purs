module Test.QuickCheck.Laws.Data.Bounded where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Ordering: `bottom <= a <= top`
checkBounded
  ∷ ∀ a
  . Arbitrary a
  ⇒ Bounded a
  ⇒ Ord a
  ⇒ Proxy a
  → Effect Unit
checkBounded _ = checkBoundedGen (arbitrary :: Gen a)

checkBoundedGen
  ∷ ∀ a
  . Bounded a
  ⇒ Ord a
  ⇒ Gen a
  → Effect Unit
checkBoundedGen gen = do
  log "Checking 'Ordering' law for Bounded"
  quickCheck' 1000 $ ordering <$> gen

  where

  ordering ∷ a → Boolean
  ordering a = bottom <= a && a <= top
