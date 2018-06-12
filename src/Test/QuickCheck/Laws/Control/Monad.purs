module Test.QuickCheck.Laws.Control.Monad where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A)
import Type.Proxy (Proxy2)

-- | - Left Identity: `pure x >>= f = f x`
-- | - Right Identity: `x >>= pure = x`
checkMonad
  ∷ ∀ m
  . Monad m
  ⇒ Arbitrary (m A)
  ⇒ Eq (m A)
  ⇒ Proxy2 m
  → Effect Unit
checkMonad _ = do

  log "Checking 'Left identity' law for Monad"
  quickCheck' 1000 leftIdentity

  log "Checking 'Right identity' law for Monad"
  quickCheck' 1000 rightIdentity

  where

  leftIdentity ∷ A → (A → m A) → Boolean
  leftIdentity x f = (pure x >>= f) == f x

  rightIdentity ∷ m A → Boolean
  rightIdentity m = (m >>= pure) == m
