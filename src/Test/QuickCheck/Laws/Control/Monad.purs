module Test.QuickCheck.Laws.Control.Monad where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A)
import Type.Proxy (Proxy)

-- | - Left Identity: `pure x >>= f = f x`
-- | - Right Identity: `x >>= pure = x`
checkMonad
  ∷ ∀ m
  . Monad m
  ⇒ Arbitrary (m A)
  ⇒ Eq (m A)
  ⇒ Proxy m
  → Effect Unit
checkMonad _ =
  checkMonadGen
    (arbitrary :: Gen (m A))
    (arbitrary :: Gen (A → m A))

checkMonadGen
  ∷ ∀ m
  . Monad m
  ⇒ Eq (m A)
  ⇒ Gen (m A)
  → Gen (A → m A)
  → Effect Unit
checkMonadGen gen genf = do
  log "Checking 'Left identity' law for Monad"
  quickCheck' 1000 $ leftIdentity <$> genf

  log "Checking 'Right identity' law for Monad"
  quickCheck' 1000 $ rightIdentity <$> gen

  where

  leftIdentity ∷ (A → m A) → A → Boolean
  leftIdentity f x = (pure x >>= f) == f x

  rightIdentity ∷ m A → Boolean
  rightIdentity m = (m >>= pure) == m
