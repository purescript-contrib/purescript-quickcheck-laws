module Test.QuickCheck.Laws.Num where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

checkNum :: forall a. (Num a, Arbitrary a, Eq a) => a -> QC Unit
checkNum _ = do

  trace "Checking 'Commutative multiplication' law for Num"
  quickCheck commutativeMultiplication

  where

  commutativeMultiplication :: a -> a -> Boolean
  commutativeMultiplication a b = a * b == b * a
