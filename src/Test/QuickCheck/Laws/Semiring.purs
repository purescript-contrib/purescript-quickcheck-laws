module Test.QuickCheck.Laws.Semiring where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

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
checkSemiring :: forall a. (Semiring a, Arbitrary a, Eq a) => a -> QC Unit
checkSemiring _ = do

  trace "Checking 'Associativity' law for Semiring addition"
  quickCheck associativeAddition

  trace "Checking 'Identity' law for Semiring addition"
  quickCheck identityAddition

  trace "Checking 'Commutative' law for Semiring addition"
  quickCheck commutativeAddition

  trace "Checking 'Associativity' law for Semiring multiplication"
  quickCheck associativeMultiplication

  trace "Checking 'Identity' law for Semiring multiplication"
  quickCheck identityMultiplication

  trace "Checking 'Left distribution' law for Semiring"
  quickCheck leftDistribution

  trace "Checking 'Right distribution' law for Semiring"
  quickCheck rightDistribution

  where

  associativeAddition :: a -> a -> a -> Boolean
  associativeAddition a b c = (a + b) + c == a + (b + c)

  identityAddition :: a -> Boolean
  identityAddition a = (zero + a == a) && (a + zero == a)

  commutativeAddition :: a -> a -> Boolean
  commutativeAddition a b = a + b == b + a

  associativeMultiplication :: a -> a -> a -> Boolean
  associativeMultiplication a b c = (a * b) * c == a * (b * c)

  identityMultiplication :: a -> Boolean
  identityMultiplication a = (one * a == a) && (a * one == a)

  leftDistribution :: a -> a -> a -> Boolean
  leftDistribution a b c = a * (b + c) == (a * b) + (a * c)

  rightDistribution :: a -> a -> a -> Boolean
  rightDistribution a b c = (a + b) * c == (a * c) + (b * c)

  annihiliation :: a -> Boolean
  annihiliation a = (a * zero == zero) && (zero * a == zero)
