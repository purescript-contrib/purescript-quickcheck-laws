module Test.QuickCheck.Laws.Control.Bind where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy2)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A)

-- | - Associativity: `(x >>= f) >>= g = x >>= (\k → f k >>= g)`
checkBind
  ∷ ∀ eff m
  . (Bind m, Arbitrary (m A), Eq (m A))
  ⇒ Proxy2 m
  → QC eff Unit
checkBind _ = do

  log "Checking 'Associativity' law for Bind"
  quickCheck' 1000 associativity

  where

  associativity ∷ m A → (A → m A) → (A → m A) → Boolean
  associativity m f g = ((m >>= f) >>= g) == (m >>= (\x → f x >>= g))
