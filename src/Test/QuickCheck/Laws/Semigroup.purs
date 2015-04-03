module Test.QuickCheck.Laws.Semigroup where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

checkSemigroup :: forall s. (Semigroup s, Arbitrary s, Eq s) => s -> QC Unit
checkSemigroup _ = do

  trace "Checking 'Associativity' law for Semigroup"
  quickCheck associativity

  where

  associativity :: s -> s -> s -> Boolean
  associativity x y z = ((x <> y) <> z) == (x <> (y <> z))
