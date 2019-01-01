module Test.QuickCheck.Laws.Data.DivisionRing where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===), (/==))
import Test.QuickCheck.Combinators ((|=|), (&=&))
import Test.QuickCheck.Arbitrary (class Arbitrary)
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
checkDivisionRing _ = do

  log "Checking 'Non-zero ring' law for DivisionRing"
  quickCheck' 1000 nonZero

  log "Checking 'Non-zero multiplicative inverse' law for DivisionRing"
  quickCheck' 1000 inverse

  where

  nonZero ∷ Boolean
  nonZero = (one :: Eq a => a) /= zero

  inverse ∷ DivisionRing a => a → Boolean
  inverse a
    | a == zero = true
    | (recip a * a == a * recip a) && (recip a * a == one) && (a * recip a == one) = true
    | otherwise = false


-- | Same as `checkDivisionRing`, but with better error reporting.
-- | Non-zero ring: one /= zero
-- | Non-zero multiplicative inverse: recip a * a = a * recip a = one for all non-zero a
checkDivisionRingShow
  ∷ ∀ a
  . DivisionRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → Effect Unit
checkDivisionRingShow _ = do

  log "Checking 'Non-zero ring' law for DivisionRing"
  quickCheck' 1000 nonZero

  log "Checking 'Non-zero multiplicative inverse' law for DivisionRing"
  quickCheck' 1000 inverse

  where

  nonZero ∷ Result
  nonZero = (one :: Eq a => a) /== zero

  inverse ∷ DivisionRing a => a → Result
  inverse a =
    let isZero = a === zero
        commRecip = recip a * a === a * recip a
        recipOneR = recip a * a === one
        recipOneL = a * recip a === one
    in  isZero |=| (commRecip &=& recipOneR &=& recipOneL)
