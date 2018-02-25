module Test.QuickCheck.Laws.Data.Semiring where

import Prelude

import Control.Apply (lift2, lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC, arbitrary, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
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
  ∷ ∀ eff a
  . Semiring a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkSemiring _ = checkSemiringGen (arbitrary :: Gen a)

checkSemiringGen
  ∷ ∀ eff a
  . Semiring a
  ⇒ Eq a
  ⇒ Gen a
  → QC eff Unit
checkSemiringGen gen = do

  log "Checking 'Associativity' law for Semiring addition"
  quickCheck' 1000 $ lift3 associativeAddition gen gen gen

  log "Checking 'Identity' law for Semiring addition"
  quickCheck' 1000 $ identityAddition <$> gen

  log "Checking 'Commutative' law for Semiring addition"
  quickCheck' 1000 $ lift2 commutativeAddition gen gen

  log "Checking 'Associativity' law for Semiring multiplication"
  quickCheck' 1000 $ lift3 associativeMultiplication gen gen gen

  log "Checking 'Identity' law for Semiring multiplication"
  quickCheck' 1000 $ identityMultiplication <$> gen

  log "Checking 'Left distribution' law for Semiring"
  quickCheck' 1000 $ lift3 leftDistribution gen gen gen

  log "Checking 'Right distribution' law for Semiring"
  quickCheck' 1000 $ lift3 rightDistribution gen gen gen

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
