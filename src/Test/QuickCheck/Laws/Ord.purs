module Test.QuickCheck.Laws.Ord where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
checkOrd :: forall a. (Arbitrary a, Ord a) => a -> QC Unit
checkOrd _ = do

  trace "Checking 'Reflexivity' law for Ord"
  quickCheck reflexivity

  trace "Checking 'Antisymmetry' law for Ord"
  quickCheck antisymmetry

  trace "Checking 'Transitivity' law for Ord"
  quickCheck transitivity

  where

  reflexivity :: a -> Boolean
  reflexivity a = a <= a

  antisymmetry :: a -> a -> Boolean
  antisymmetry a b = if a <= b && b <= a then a == b else a /= b

  transitivity :: a -> a -> a -> Boolean
  transitivity a b c = if a <= b && b <= c then a <= c else c > a
