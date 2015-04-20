module Test.QuickCheck.Laws.Data.Lattice where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

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
checkLattice :: forall a. (Arbitrary a, Lattice a) => Proxy a -> QC Unit
checkLattice _ = do

  log "Checking 'Associativity' law for Lattice"
  quickCheck associativity

  log "Checking 'Commutativity' law for Lattice"
  quickCheck commutativity

  log "Checking 'Absorption' law for Lattice"
  quickCheck absorption

  log "Checking 'Idempotent' law for Lattice"
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
