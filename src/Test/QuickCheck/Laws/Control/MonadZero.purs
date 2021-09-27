module Test.QuickCheck.Laws.Control.MonadZero where

import Prelude

import Control.MonadZero (class MonadZero)
import Control.Plus (empty)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
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
checkMonadZero _ = checkMonadZeroGen (arbitrary ∷ Gen (A → m B))

checkMonadZeroGen
  ∷ ∀ m
  . MonadZero m
  ⇒ Eq (m B)
  ⇒ Gen (A → m B)
  → Effect Unit
checkMonadZeroGen gen = do

  log "Checking 'Annihilation' law for MonadZero"
  quickCheck' 1000 $ annihilation <$> gen

  where

  annihilation ∷ (A → m B) → Boolean
  annihilation f = (empty >>= f) == empty
