module Test.QuickCheck.Laws.Data.Ord where

import Prelude

import Control.Apply (lift2, lift3)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
checkOrd
  ∷ ∀ a
  . Arbitrary a
  ⇒ Ord a
  ⇒ Proxy a
  → Effect Unit
checkOrd _ = checkOrdGen (arbitrary :: Gen a)

checkOrdGen
  ∷ ∀ a
  . Ord a
  ⇒ Gen a
  → Effect Unit
checkOrdGen gen = do
  log "Checking 'Reflexivity' law for Ord"
  quickCheck' 1000 $ reflexivity <$> gen

  log "Checking 'Antisymmetry' law for Ord"
  quickCheck' 1000 $ lift2 antisymmetry gen gen

  log "Checking 'Transitivity' law for Ord"
  quickCheck' 1000 $ lift3 transitivity gen gen gen

  where

  reflexivity ∷ a → Boolean
  reflexivity a = a <= a

  antisymmetry ∷ a → a → Boolean
  antisymmetry a b = if (a <= b) && (b <= a) then a == b else a /= b

  transitivity ∷ a → a → a → Boolean
  transitivity a b c = if (a <= b) && (b <= c) then a <= c else true
