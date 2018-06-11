module Test.QuickCheck.Laws.Data.CommutativeRing where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Type.Proxy (Proxy)

import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

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
