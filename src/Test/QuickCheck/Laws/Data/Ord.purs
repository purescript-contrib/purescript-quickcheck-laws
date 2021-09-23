module Test.QuickCheck.Laws.Data.Ord where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
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
checkOrd _ = do

  log "Checking 'Reflexivity' law for Ord"
  quickCheck' 1000 reflexivity

  log "Checking 'Antisymmetry' law for Ord"
  quickCheck' 1000 antisymmetry

  log "Checking 'Transitivity' law for Ord"
  quickCheck' 1000 transitivity

  where

  reflexivity ∷ a → Boolean
  reflexivity a = a <= a

  antisymmetry ∷ a → a → Boolean
  antisymmetry a b = if (a <= b) && (b <= a) then a == b else a /= b

  transitivity ∷ a → a → a → Boolean
  transitivity a b c = if (a <= b) && (b <= c) then a <= c else true
