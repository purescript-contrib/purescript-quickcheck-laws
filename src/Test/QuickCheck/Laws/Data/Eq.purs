module Test.QuickCheck.Laws.Data.Eq where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- | - Negation: `x /= y = not (x == y)`
checkEq :: forall a. (Arbitrary a, Eq a) => Proxy a -> QC Unit
checkEq _ = do

  log "Checking 'Reflexivity' law for Eq"
  quickCheck reflexivity

  log "Checking 'Symmetry' law for Eq"
  quickCheck symmetry

  log "Checking 'Transitivity' law for Eq"
  quickCheck transitivity

  log "Checking 'Negation' law for Eq"
  quickCheck negation

  where

  reflexivity :: a -> Boolean
  reflexivity x = (x == x) == true

  symmetry :: a -> a -> Boolean
  symmetry x y = (x == y) == (y == x)

  transitivity :: a -> a -> a -> Boolean
  transitivity x y z = if (x == y) && (y == z) then x == z else true

  negation :: a -> a -> Boolean
  negation x y = (x /= y) == not (x == y)
