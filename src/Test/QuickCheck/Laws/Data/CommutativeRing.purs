module Test.QuickCheck.Laws.Data.CommutativeRing where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

-- | - Commutative multiplication: `a * b = b * a`
checkCommutativeRing
  ∷ ∀ a
  . CommutativeRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkCommutativeRing _ = do

  log "Checking 'Commutative multiplication' law for CommutativeRing"
  quickCheck' 1000 commutativeMultiplication

  where

  commutativeMultiplication ∷ a → a → Boolean
  commutativeMultiplication a b = a * b == b * a


-- | Same as `checkCommutativeRing`, but with better error reporting.
-- | - Commutative multiplication: `a * b = b * a`
checkCommutativeRingShow
  ∷ ∀ a
  . CommutativeRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → Effect Unit
checkCommutativeRingShow _ = do

  log "Checking 'Commutative multiplication' law for CommutativeRing"
  quickCheck' 1000 commutativeMultiplication

  where

  commutativeMultiplication ∷ a → a → Result
  commutativeMultiplication a b = a * b === b * a
