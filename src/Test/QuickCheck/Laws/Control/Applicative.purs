module Test.QuickCheck.Laws.Control.Applicative where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

-- | - Identity: `(pure id) <*> v = v`
-- | - Composition: `(pure (<<<)) <*> f <*> g <*> h = f <*> (g <*> h)`
-- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
-- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
checkApplicative :: forall f a b c. (Applicative f,
                                     Arbitrary a,
                                     Arbitrary b,
                                     Arbitrary (f a),
                                     Arbitrary (f (a -> b)),
                                     Arbitrary (f (b -> c)),
                                     Coarbitrary a,
                                     Eq (f a),
                                     Eq (f b),
                                     Eq (f c)) => Proxy2 f
                                               -> Proxy a
                                               -> Proxy b
                                               -> Proxy c
                                               -> QC Unit
checkApplicative _ _ _ _ = do

  log "Checking 'Identity' law for Applicative"
  quickCheck identity

  log "Checking 'Composition' law for Applicative"
  quickCheck composition

  log "Checking 'Homomorphism' law for Applicative"
  quickCheck homomorphism

  log "Checking 'Interchange' law for Applicative"
  quickCheck interchange

  where

  identity :: f a -> Boolean
  identity v = (pure id <*> v) == v

  composition :: f (b -> c) -> f (a -> b) -> f a -> Boolean
  composition f g x = (pure (<<<) <*> f <*> g <*> x) == (f <*> (g <*> x))

  homomorphism :: (a -> b) -> a -> Boolean
  homomorphism f x = (pure f <*> pure x) == (pure (f x) :: f b)

  interchange :: a -> f (a -> b) -> Boolean
  interchange y u = (u <*> pure y) == (pure ($ y) <*> u)
