module Test.QuickCheck.Laws.Data.CommutativeRing where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Commutative multiplication: `a * b = b * a`
checkCommutativeRing
  ∷ ∀ eff a
  . CommutativeRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkCommutativeRing _ = checkCommutativeRingGen (arbitrary :: Gen a)

checkCommutativeRingGen
  ∷ ∀ eff a
  . CommutativeRing a
  ⇒ Eq a
  ⇒ Gen a
  → QC eff Unit
checkCommutativeRingGen gen = do

  log "Checking 'Commutative multiplication' law for CommutativeRing"
  quickCheck' 1000 $ lift2 commutativeMultiplication gen gen

  where

  commutativeMultiplication ∷ a → a → Boolean
  commutativeMultiplication a b = a * b == b * a
