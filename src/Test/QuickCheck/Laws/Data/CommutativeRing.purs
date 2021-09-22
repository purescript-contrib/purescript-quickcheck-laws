module Test.QuickCheck.Laws.Data.CommutativeRing where

import Prelude

import Control.Apply (lift2)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Commutative multiplication: `a * b = b * a`
checkCommutativeRing
  ∷ ∀ a
  . CommutativeRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkCommutativeRing _ = checkCommutativeRingGen (arbitrary :: Gen a)

checkCommutativeRingGen
  ∷ ∀ a
  . CommutativeRing a
  ⇒ Eq a
  ⇒ Gen a
  → Effect Unit
checkCommutativeRingGen gen = do
  log "Checking 'Commutative multiplication' law for CommutativeRing"
  quickCheck' 1000 $ lift2 commutativeMultiplication gen gen

  where

  commutativeMultiplication ∷ a → a → Boolean
  commutativeMultiplication a b = a * b == b * a
