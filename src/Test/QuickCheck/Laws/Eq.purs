module Test.QuickCheck.Laws.Eq where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- | - Negation: `x /= y = not (x == y)`
checkEq :: forall a. (Arbitrary a, Eq a) => a -> QC Unit
checkEq _ = do

  trace "Checking 'Reflexivity' law for Eq"
  quickCheck reflexivity

  trace "Checking 'Symmetry' law for Eq"
  quickCheck symmetry

  trace "Checking 'Transitivity' law for Eq"
  quickCheck transitivity

  trace "Checking 'Negation' law for Eq"
  quickCheck negation

  where

  reflexivity :: a -> Boolean
  reflexivity x = (x == x) == true

  symmetry :: a -> a -> Boolean
  symmetry x y = (x == y) == (y == x)

  transitivity :: a -> a -> a -> Boolean
  transitivity x y z = (x == y) == (y == z) == (x == z)

  negation :: a -> a -> Boolean
  negation x y = (x /= x) == not (x == y)
