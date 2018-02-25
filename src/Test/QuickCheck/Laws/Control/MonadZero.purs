module Test.QuickCheck.Laws.Control.MonadZero where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.MonadZero (class MonadZero)
import Control.Plus (empty)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Annihilation: `empty >>= f = empty`
checkMonadZero
  ∷ ∀ eff m
  . MonadZero m
  ⇒ Arbitrary (m A)
  ⇒ Arbitrary (m B)
  ⇒ Eq (m B)
  ⇒ Proxy2 m
  → QC eff Unit
checkMonadZero _ = checkMonadZeroGen (arbitrary ∷ Gen (A → m B))

checkMonadZeroGen
  ∷ ∀ eff m
  . MonadZero m
  ⇒ Eq (m B)
  ⇒ Gen (A → m B)
  → QC eff Unit
checkMonadZeroGen gen = do

  log "Checking 'Annihilation' law for MonadZero"
  quickCheck' 1000 $ annihilation <$> gen

  where

  annihilation ∷ (A → m B) → Boolean
  annihilation f = (empty >>= f) == empty
