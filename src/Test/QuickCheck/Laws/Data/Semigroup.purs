module Test.QuickCheck.Laws.Data.Semigroup where

import Prelude

import Control.Apply (lift3)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
checkSemigroup
  ∷ ∀ s
  . Semigroup s
  ⇒ Arbitrary s
  ⇒ Eq s
  ⇒ Proxy s
  → Effect Unit
checkSemigroup _ = checkSemigroupGen (arbitrary :: Gen s)

checkSemigroupGen
  ∷ ∀ s
  . Semigroup s
  ⇒ Eq s
  ⇒ Gen s
  → Effect Unit
checkSemigroupGen gen = do
  log "Checking 'Associativity' law for Semigroup"
  quickCheck' 1000 $ lift3 associativity gen gen gen

  where

  associativity ∷ s → s → s → Boolean
  associativity x y z = ((x <> y) <> z) == (x <> (y <> z))
