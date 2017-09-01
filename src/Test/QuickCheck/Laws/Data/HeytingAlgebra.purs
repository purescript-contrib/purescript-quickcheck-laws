module Test.QuickCheck.Laws.Data.HeytingAlgebra where

import Prelude

import Control.Monad.Eff.Console (log)

import Data.HeytingAlgebra (tt, ff, implies)

import Type.Proxy (Proxy)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

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
-- | - Identity:
-- |   - `a || ff = a`
-- |   - `a && tt = a`
-- | - Implication:
-- |   - ``a `implies` a = tt``
-- |   - ``a && (a `implies` b) = a && b``
-- |   - ``b && (a `implies` b) = b``
-- |   - ``a `implies` (b && c) = (a `implies` b) && (a `implies` c)``
-- | - Complemented:
-- |   - ``not a = a `implies` ff``
checkHeytingAlgebra
  ∷ ∀ eff a
  . Arbitrary a
  ⇒ HeytingAlgebra a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkHeytingAlgebra _ = do

  log "Checking 'Associativity of disjunction' law for HeytingAlgebra"
  quickCheck' 1000 (associativity (||))

  log "Checking 'Associativity of conjunction' law for HeytingAlgebra"
  quickCheck' 1000 (associativity (&&))

  log "Checking 'Commutativity of disjunction' law for HeytingAlgebra"
  quickCheck' 1000 (commutativity (||))

  log "Checking 'Commutativity of conjunction' law for HeytingAlgebra"
  quickCheck' 1000 (commutativity (&&))

  log "Checking 'Absorption of disjunction' law for HeytingAlgebra"
  quickCheck' 1000 (absorption (||) (&&))

  log "Checking 'Absorption of conjunction' law for HeytingAlgebra"
  quickCheck' 1000 (absorption (&&) (||))

  log "Checking 'Idempotent disjunction' law for HeytingAlgebra"
  quickCheck' 1000 (idempotent (||))

  log "Checking 'Idempotent conjunction' law for HeytingAlgebra"
  quickCheck' 1000 (idempotent (&&))

  log "Checking 'Disjunction identity' law for HeytingAlgebra"
  quickCheck' 1000 (identity (||) ff)

  log "Checking 'Conjunction identity' law for HeytingAlgebra"
  quickCheck' 1000 (identity (&&) tt)

  log "Checking 'Implication' laws for HeytingAlgebra"
  quickCheck' 1000 implicationId
  quickCheck' 1000 implications
  quickCheck' 1000 distributiveImplication

  log "Checking 'Complemented' law for HeytingAlgebra"
  quickCheck' 1000 complemented

  where

  associativity ∷ (a → a → a) → a → a → a → Boolean
  associativity op a b c = (a `op` (b `op` c)) == ((a `op` b) `op` c)

  commutativity ∷ (a → a → a) → a → a → Boolean
  commutativity op a b = (a `op` b) == (b `op` a)

  absorption ∷ (a → a → a) → (a → a → a) → a → a → Boolean
  absorption op1 op2 a b = (a `op1` (a `op2` b)) == a

  idempotent ∷ (a → a → a) → a → a → Boolean
  idempotent op a b = a `op` a == a

  identity ∷ (a → a → a) → a → a → Boolean
  identity op ident a = a `op` ident == a

  implicationId ∷ a → Boolean
  implicationId a = (a `implies` a) == tt

  implications ∷ a → a → Boolean
  implications a b
    = ((a && (a `implies` b)) == (a && b))
    && ((b && (a `implies` b)) == b)

  distributiveImplication ∷ a → a → a → Boolean
  distributiveImplication a b c = (a `implies` (b && c)) == ((a `implies` b) && (a `implies` c))

  complemented ∷ a → Boolean
  complemented a = not a == (a `implies` ff)
