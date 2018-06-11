module Test.QuickCheck.Laws.Data.Semigroup where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Type.Proxy (Proxy)

import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
checkSemigroup
  ∷ ∀ s
  . Semigroup s
  ⇒ Arbitrary s
  ⇒ Eq s
  ⇒ Proxy s
  → Effect Unit
checkSemigroup _ = do

  log "Checking 'Associativity' law for Semigroup"
  quickCheck' 1000 associativity

  where

  associativity ∷ s → s → s → Boolean
  associativity x y z = ((x <> y) <> z) == (x <> (y <> z))
