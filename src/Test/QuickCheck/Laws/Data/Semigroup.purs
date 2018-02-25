module Test.QuickCheck.Laws.Data.Semigroup where

import Prelude

import Control.Apply (lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, arbitrary, QC, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
checkSemigroup
  ∷ ∀ eff s
  . Semigroup s
  ⇒ Arbitrary s
  ⇒ Eq s
  ⇒ Proxy s
  → QC eff Unit
checkSemigroup _ = checkSemigroupGen (arbitrary :: Gen s)

checkSemigroupGen
  ∷ ∀ eff s
  . Semigroup s
  ⇒ Eq s
  ⇒ Gen s
  → QC eff Unit
checkSemigroupGen gen = do

  log "Checking 'Associativity' law for Semigroup"
  quickCheck' 1000 $ lift3 associativity gen gen gen

  where

  associativity ∷ s → s → s → Boolean
  associativity x y z = ((x <> y) <> z) == (x <> (y <> z))
