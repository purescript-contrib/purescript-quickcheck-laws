module Test.QuickCheck.Laws.Control.MonadZero where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.MonadZero (class MonadZero)
import Control.Plus (empty)

import Type.Proxy (Proxy2)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)

-- | - Annihilation: `empty >>= f = empty`
checkMonadZero
  ∷ ∀ eff m
  . MonadZero m
  ⇒ Arbitrary (m A)
  ⇒ Arbitrary (m B)
  ⇒ Eq (m B)
  ⇒ Proxy2 m
  → QC eff Unit
checkMonadZero _ = do

  log "Checking 'Annihilation' law for MonadZero"
  quickCheck' 1000 annihilation

  where

  annihilation ∷ (A → m B) → Boolean
  annihilation f = (empty >>= f) == empty
