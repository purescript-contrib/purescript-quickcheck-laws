module Test.QuickCheck.Laws.Control.MonadZero where

import Prelude

import Control.MonadZero (class MonadZero)
import Control.Plus (empty)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Annihilation: `empty >>= f = empty`
checkMonadZero
  ∷ ∀ m
  . MonadZero m
  ⇒ Arbitrary (m A)
  ⇒ Arbitrary (m B)
  ⇒ Eq (m B)
  ⇒ Proxy2 m
  → Effect Unit
checkMonadZero _ = do

  log "Checking 'Annihilation' law for MonadZero"
  quickCheck' 1000 annihilation

  where

  annihilation ∷ (A → m B) → Boolean
  annihilation f = (empty >>= f) == empty


-- | Like `checkMonadZero`, but with better error reporting.
-- | - Annihilation: `empty >>= f = empty`
checkMonadZeroShow
  ∷ ∀ m
  . MonadZero m
  ⇒ Arbitrary (m A)
  ⇒ Arbitrary (m B)
  ⇒ Eq (m B)
  ⇒ Show (m B)
  ⇒ Proxy2 m
  → Effect Unit
checkMonadZeroShow _ = do

  log "Checking 'Annihilation' law for MonadZero"
  quickCheck' 1000 annihilation

  where

  annihilation ∷ (A → m B) → Result
  annihilation f = (empty >>= f) === empty
