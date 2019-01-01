module Test.QuickCheck.Laws.Data.Semiring where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Combinators ((&=&))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

-- | - Commutative monoid under addition:
-- |   - Associativity: `(a + b) + c = a + (b + c)`
-- |   - Identity: `zero + a = a + zero = a`
-- |   - Commutative: `a + b = b + a`
-- | - Monoid under multiplication:
-- |   - Associativity: `(a * b) * c = a * (b * c)`
-- |   - Identity: `one * a = a * one = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
-- |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
-- | - Annihiliation: `zero * a = a * zero = zero`
checkSemiring
  ∷ ∀ a
  . Semiring a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkSemiring _ = do

  log "Checking 'Associativity' law for Semiring addition"
  quickCheck' 1000 associativeAddition

  log "Checking 'Identity' law for Semiring addition"
  quickCheck' 1000 identityAddition

  log "Checking 'Commutative' law for Semiring addition"
  quickCheck' 1000 commutativeAddition

  log "Checking 'Associativity' law for Semiring multiplication"
  quickCheck' 1000 associativeMultiplication

  log "Checking 'Identity' law for Semiring multiplication"
  quickCheck' 1000 identityMultiplication

  log "Checking 'Left distribution' law for Semiring"
  quickCheck' 1000 leftDistribution

  log "Checking 'Right distribution' law for Semiring"
  quickCheck' 1000 rightDistribution

  where

  associativeAddition ∷ a → a → a → Boolean
  associativeAddition a b c = (a + b) + c == a + (b + c)

  identityAddition ∷ a → Boolean
  identityAddition a = (zero + a == a) && (a + zero == a)

  commutativeAddition ∷ a → a → Boolean
  commutativeAddition a b = a + b == b + a

  associativeMultiplication ∷ a → a → a → Boolean
  associativeMultiplication a b c = (a * b) * c == a * (b * c)

  identityMultiplication ∷ a → Boolean
  identityMultiplication a = (one * a == a) && (a * one == a)

  leftDistribution ∷ a → a → a → Boolean
  leftDistribution a b c = a * (b + c) == (a * b) + (a * c)

  rightDistribution ∷ a → a → a → Boolean
  rightDistribution a b c = (a + b) * c == (a * c) + (b * c)

  annihiliation ∷ a → Boolean
  annihiliation a = (a * zero == zero) && (zero * a == zero)


-- | Like `checkSemiring`, but with better error reporting
-- | - Commutative monoid under addition:
-- |   - Associativity: `(a + b) + c = a + (b + c)`
-- |   - Identity: `zero + a = a + zero = a`
-- |   - Commutative: `a + b = b + a`
-- | - Monoid under multiplication:
-- |   - Associativity: `(a * b) * c = a * (b * c)`
-- |   - Identity: `one * a = a * one = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
-- |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
-- | - Annihiliation: `zero * a = a * zero = zero`
checkSemiringShow
  ∷ ∀ a
  . Semiring a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → Effect Unit
checkSemiringShow _ = do

  log "Checking 'Associativity' law for Semiring addition"
  quickCheck' 1000 associativeAddition

  log "Checking 'Identity' law for Semiring addition"
  quickCheck' 1000 identityAddition

  log "Checking 'Commutative' law for Semiring addition"
  quickCheck' 1000 commutativeAddition

  log "Checking 'Associativity' law for Semiring multiplication"
  quickCheck' 1000 associativeMultiplication

  log "Checking 'Identity' law for Semiring multiplication"
  quickCheck' 1000 identityMultiplication

  log "Checking 'Left distribution' law for Semiring"
  quickCheck' 1000 leftDistribution

  log "Checking 'Right distribution' law for Semiring"
  quickCheck' 1000 rightDistribution

  where

  associativeAddition ∷ a → a → a → Result
  associativeAddition a b c = (a + b) + c === a + (b + c)

  identityAddition ∷ a → Result
  identityAddition a = (zero + a === a) &=& (a + zero === a)

  commutativeAddition ∷ a → a → Result
  commutativeAddition a b = a + b === b + a

  associativeMultiplication ∷ a → a → a → Result
  associativeMultiplication a b c = (a * b) * c === a * (b * c)

  identityMultiplication ∷ a → Result
  identityMultiplication a = (one * a === a) &=& (a * one === a)

  leftDistribution ∷ a → a → a → Result
  leftDistribution a b c = a * (b + c) === (a * b) + (a * c)

  rightDistribution ∷ a → a → a → Result
  rightDistribution a b c = (a + b) * c === (a * c) + (b * c)

  annihiliation ∷ a → Result
  annihiliation a = (a * zero === zero) &=& (zero * a === zero)
