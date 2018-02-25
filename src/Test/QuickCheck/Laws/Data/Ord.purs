module Test.QuickCheck.Laws.Data.Ord where

import Prelude

import Control.Apply (lift2, lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
checkOrd
  ∷ ∀ eff a
  . Arbitrary a
  ⇒ Ord a
  ⇒ Proxy a
  → QC eff Unit
checkOrd _ = checkOrdGen (arbitrary :: Gen a)

checkOrdGen
  ∷ ∀ eff a
  . Ord a
  ⇒ Gen a
  → QC eff Unit
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
