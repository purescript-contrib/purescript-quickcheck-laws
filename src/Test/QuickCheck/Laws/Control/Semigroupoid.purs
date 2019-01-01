module Test.QuickCheck.Laws.Control.Semigroupoid where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (B, C, D, E)
import Type.Proxy (Proxy3)

-- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
checkSemigroupoid
  ∷ ∀ a
  . Semigroupoid a
  ⇒ Arbitrary (a B C)
  ⇒ Arbitrary (a C D)
  ⇒ Arbitrary (a D E)
  ⇒ Eq (a B E)
  ⇒ Proxy3 a
  → Effect Unit
checkSemigroupoid _ = do

  log "Checking 'Associativity' law for Semigroupoid"
  quickCheck' 1000 associativity

  where

  associativity ∷ a D E → a C D → a B C → Boolean
  associativity p q r = (p <<< (q <<< r)) == ((p <<< q) <<< r)


-- | Like `checkSemigroupoid`, but with better error reporting.
-- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
checkSemigroupoidShow
  ∷ ∀ a
  . Semigroupoid a
  ⇒ Arbitrary (a B C)
  ⇒ Arbitrary (a C D)
  ⇒ Arbitrary (a D E)
  ⇒ Eq (a B E)
  ⇒ Show (a B E)
  ⇒ Proxy3 a
  → Effect Unit
checkSemigroupoidShow _ = do

  log "Checking 'Associativity' law for Semigroupoid"
  quickCheck' 1000 associativity

  where

  associativity ∷ a D E → a C D → a B C → Result
  associativity p q r = (p <<< (q <<< r)) === ((p <<< q) <<< r)
