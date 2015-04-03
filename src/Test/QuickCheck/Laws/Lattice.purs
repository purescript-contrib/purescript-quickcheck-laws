module Test.QuickCheck.Laws.Lattice where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Associativity:
-- |   - `a || (b || c) = (a || b) || c`
-- |   - `a && (b && c) = (a && b) && c`
-- | - Commutativity:
-- |   - `a || b = b || a`
-- |   - `a && b = b && a`
-- | - Absorption:
-- |   - `a || (a && b) = a`
-- |   - `a && (a || b) = a`
-- | - Idempotent:
-- |   - `a || a = a`
-- |   - `a && a = a`
checkLattice :: forall m a. (Arbitrary a, Lattice a) => a -> QC Unit
checkLattice _ = do

  trace "Checking 'Associativity' law for Lattice"
  quickCheck associativity

  trace "Checking 'Commutativity' law for Lattice"
  quickCheck commutativity

  trace "Checking 'Absorption' law for Lattice"
  quickCheck absorption

  trace "Checking 'Idempotent' law for Lattice"
  quickCheck idempotent

  where

  associativity :: a -> a -> a -> Boolean
  associativity a b c = (a || (b || c)) == ((a || b) || c)
                     && (a && (b && c)) == ((a && b) && c)

  commutativity :: a -> a -> Boolean
  commutativity a b = (a || b) == (b || a)
                   && (a && b) == (b && a)

  absorption :: a -> a -> Boolean
  absorption a b = (a || (a && b)) == a
                && (a && (a || b)) == a

  idempotent :: a -> Boolean
  idempotent a = (a || a) == a
              && (a && a) == a
