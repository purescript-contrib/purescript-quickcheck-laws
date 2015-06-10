module Test.QuickCheck.Laws.Control.Applicative where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Identity: `(pure id) <*> v = v`
-- | - Composition: `(pure (<<<)) <*> f <*> g <*> h = f <*> (g <*> h)`
-- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
-- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
checkApplicative :: forall f. (Applicative f, Arbitrary1 f, Eq1 f) => Proxy2 f -> QC Unit
checkApplicative _ = do

  log "Checking 'Identity' law for Applicative"
  quickCheck identity

  log "Checking 'Composition' law for Applicative"
  quickCheck composition

  log "Checking 'Homomorphism' law for Applicative"
  quickCheck homomorphism

  log "Checking 'Interchange' law for Applicative"
  quickCheck interchange

  where

  identity :: Wrap f A -> Boolean
  identity (Wrap v) = (pure id <*> v) `eq1` v

  composition :: Wrap f (A -> C) -> Wrap f (A -> B) -> Wrap f A -> Boolean
  composition (Wrap f) (Wrap g) (Wrap x) = (pure (<<<) <*> f <*> g <*> x) `eq1` (f <*> (g <*> x))

  homomorphism :: (A -> B) -> A -> Boolean
  homomorphism f x = (pure f <*> pure x) `eq1` (pure (f x) :: f B)

  interchange :: A -> Wrap f (A -> B) -> Boolean
  interchange y (Wrap u) = (u <*> pure y) `eq1` (pure ($ y) <*> u)
