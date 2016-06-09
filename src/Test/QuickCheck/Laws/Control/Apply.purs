module Test.QuickCheck.Laws.Control.Apply where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy2)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B, C)

-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
checkApply
  ∷ ∀ eff f
  . (Apply f, Arbitrary (f A), Arbitrary (f (A → B)), Arbitrary (f (B → C)), Eq (f C))
  ⇒ Proxy2 f
  → QC eff Unit
checkApply _ = do

  log "Checking 'Associative composition' law for Apply"
  quickCheck' 1000 associativeComposition

  where

  associativeComposition ∷ f (B → C) → f (A → B) → f A → Boolean
  associativeComposition f g x = ((<<<) <$> f <*> g <*> x) == (f <*> (g <*> x))
