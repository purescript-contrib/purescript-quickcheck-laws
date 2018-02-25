module Test.QuickCheck.Laws.Data.Ring where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, arbitrary, QC, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Additive inverse: `a - a = a + (-a) = (-a) + a = zero`
checkRing
  ∷ ∀ eff a
  . Ring a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkRing _ = checkRingGen (arbitrary :: Gen a)

checkRingGen
  ∷ ∀ eff a
  . Ring a
  ⇒ Eq a
  ⇒ Gen a
  → QC eff Unit
checkRingGen gen = do

  log "Checking 'Additive inverse' law for Ring"
  quickCheck' 1000 $ additiveInverse <$> gen

  where

  additiveInverse ∷ a → Boolean
  additiveInverse a = a - a == zero && a + (-a) == zero && (-a) + a == zero
