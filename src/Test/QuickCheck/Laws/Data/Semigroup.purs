module Test.QuickCheck.Laws.Data.Semigroup where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
checkSemigroup
  ∷ ∀ eff s
  . (Semigroup s, Arbitrary s, Eq s)
  ⇒ Proxy s
  → QC eff Unit
checkSemigroup _ = do

  log "Checking 'Associativity' law for Semigroup"
  quickCheck' 1000 associativity

  where

  associativity ∷ s → s → s → Boolean
  associativity x y z = ((x <> y) <> z) == (x <> (y <> z))
