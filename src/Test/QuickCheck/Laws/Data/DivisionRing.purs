module Test.QuickCheck.Laws.Data.DivisionRing where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | Non-zero ring: one /= zero
-- | Non-zero multiplicative inverse: recip a * a = a * recip a = one for all non-zero a
checkDivisionRing
  ∷ ∀ a
  . DivisionRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkDivisionRing _ = checkDivisionRingGen (arbitrary :: Gen a)

checkDivisionRingGen
  ∷ ∀ a
  . DivisionRing a
  ⇒ Eq a
  ⇒ Gen a
  → Effect Unit
checkDivisionRingGen gen = do

  log "Checking 'Non-zero ring' law for DivisionRing"
  quickCheck' 1000 nonZero

  log "Checking 'Non-zero multiplicative inverse' law for DivisionRing"
  quickCheck' 1000 $ inverse <$> gen

  where

  nonZero ∷ Boolean
  nonZero = (one :: Eq a => a) /= zero

  inverse ∷ DivisionRing a => a → Boolean
  inverse a
    | a == zero = true
    | (recip a * a == a * recip a) && (recip a * a == one) && (a * recip a == one) = true
    | otherwise = false
