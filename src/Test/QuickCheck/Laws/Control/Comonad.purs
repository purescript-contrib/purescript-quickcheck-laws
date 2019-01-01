module Test.QuickCheck.Laws.Control.Comonad where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend ((<<=))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Left Identity: `extract <<= x = x`
-- | - Right Identity: `extract (f <<= x) = f x`
checkComonad
  ∷ ∀ w
  . Comonad w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Eq (w A)
  ⇒ Proxy2 w
  → Effect Unit
checkComonad _ = do

  log "Checking 'Left identity' law for Comonad"
  quickCheck' 1000 leftIdentity

  log "Checking 'Right identity' law for Comonad"
  quickCheck' 1000 rightIdentity

  where

  leftIdentity ∷ w A → Boolean
  leftIdentity x = (extract <<= x) == x

  rightIdentity ∷ (w A → B) → w A → Boolean
  rightIdentity f x = extract (f <<= x) == f x


-- | Like `checkComonad`, but with better error reporting.
-- | - Left Identity: `extract <<= x = x`
-- | - Right Identity: `extract (f <<= x) = f x`
checkComonadShow
  ∷ ∀ w
  . Comonad w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Eq (w A)
  ⇒ Show (w A)
  ⇒ Proxy2 w
  → Effect Unit
checkComonadShow _ = do

  log "Checking 'Left identity' law for Comonad"
  quickCheck' 1000 leftIdentity

  log "Checking 'Right identity' law for Comonad"
  quickCheck' 1000 rightIdentity

  where

  leftIdentity ∷ w A → Result
  leftIdentity x = (extract <<= x) === x

  rightIdentity ∷ (w A → B) → w A → Result
  rightIdentity f x = extract (f <<= x) === f x
