module Test.QuickCheck.Laws.Data.Eq where

import Prelude

import Control.Apply (lift2, lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- | - Negation: `x /= y = not (x == y)`
checkEq
  ∷ ∀ eff a
  . Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkEq _ = checkEqGen (arbitrary :: Gen a)

checkEqGen
  ∷ ∀ eff a
  . Eq a
  ⇒ Gen a
  → QC eff Unit
checkEqGen gen = do

  log "Checking 'Reflexivity' law for Eq"
  quickCheck' 1000 $ reflexivity <$> gen

  log "Checking 'Symmetry' law for Eq"
  quickCheck' 1000 $ lift2 symmetry gen gen

  log "Checking 'Transitivity' law for Eq"
  quickCheck' 1000 $ lift3 transitivity gen gen gen

  log "Checking 'Negation' law for Eq"
  quickCheck' 1000 $ lift2 negation gen gen

  where

  reflexivity ∷ a → Boolean
  reflexivity x = (x == x) == true

  symmetry ∷ a → a → Boolean
  symmetry x y = (x == y) == (y == x)

  transitivity ∷ a → a → a → Boolean
  transitivity x y z = if (x == y) && (y == z) then x == z else true

  negation ∷ a → a → Boolean
  negation x y = (x /= y) == not (x == y)
