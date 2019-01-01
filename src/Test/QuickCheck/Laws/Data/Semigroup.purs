module Test.QuickCheck.Laws.Data.Semigroup where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

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


-- | Like `checkSemigroup`, but with better error reporting.
-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
checkSemigroupShow
  ∷ ∀ s
  . Semigroup s
  ⇒ Arbitrary s
  ⇒ Eq s
  ⇒ Show s
  ⇒ Proxy s
  → Effect Unit
checkSemigroupShow _ = do

  log "Checking 'Associativity' law for Semigroup"
  quickCheck' 1000 associativity

  where

  associativity ∷ s → s → s → Result
  associativity x y z = ((x <> y) <> z) === (x <> (y <> z))
