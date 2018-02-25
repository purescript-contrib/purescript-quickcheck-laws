module Test.QuickCheck.Laws.Control.Semigroupoid where

import Prelude

import Control.Apply (lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC, arbitrary, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (B, C, D, E)
import Type.Proxy (Proxy3)

-- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
checkSemigroupoid
  ∷ ∀ eff a
  . Semigroupoid a
  ⇒ Arbitrary (a B C)
  ⇒ Arbitrary (a C D)
  ⇒ Arbitrary (a D E)
  ⇒ Eq (a B E)
  ⇒ Proxy3 a
  → QC eff Unit
checkSemigroupoid _ =
  checkSemigroupoidGen
    (arbitrary ∷ Gen (a B C))
    (arbitrary ∷ Gen (a C D))
    (arbitrary ∷ Gen (a D E))

checkSemigroupoidGen
  ∷ ∀ eff a
  . Semigroupoid a
  ⇒ Eq (a B E)
  ⇒ Gen (a B C)
  → Gen (a C D)
  → Gen (a D E)
  → QC eff Unit
checkSemigroupoidGen genbc gencd gende = do

  log "Checking 'Associativity' law for Semigroupoid"
  quickCheck' 1000 $ lift3 associativity gende gencd genbc

  where

  associativity ∷ a D E → a C D → a B C → Boolean
  associativity p q r = (p <<< (q <<< r)) == ((p <<< q) <<< r)
