module Test.QuickCheck.Laws.Control.Applicative where

import Prelude

import Data.Function as F
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy2)

-- | - Identity: `(pure identity) <*> v = v`
-- | - Composition: `(pure (<<<)) <*> f <*> g <*> h = f <*> (g <*> h)`
-- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
-- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
checkApplicative
  ∷ ∀ f
  . Applicative f
  ⇒ Arbitrary (f A)
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f (B → C))
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Eq (f C)
  ⇒ Proxy2 f
  → Effect Unit
checkApplicative _ = do

  log "Checking 'Identity' law for Applicative"
  quickCheck' 1000 identity

  log "Checking 'Composition' law for Applicative"
  quickCheck' 1000 composition

  log "Checking 'Homomorphism' law for Applicative"
  quickCheck' 1000 homomorphism

  log "Checking 'Interchange' law for Applicative"
  quickCheck' 1000 interchange

  where

  identity ∷ f A → Boolean
  identity v = (pure F.identity <*> v) == v

  composition ∷ f (B → C) → f (A → B) → f A → Boolean
  composition f g x = (pure (<<<) <*> f <*> g <*> x) == (f <*> (g <*> x))

  homomorphism ∷ (A → B) → A → Boolean
  homomorphism f x = (pure f <*> pure x) == (pure (f x) ∷ f B)

  interchange ∷ A → f (A → B) → Boolean
  interchange y u = (u <*> pure y) == (pure (_ $ y) <*> u)


-- | Like `checkApplicative`, but with better error reporting.
-- | - Identity: `(pure identity) <*> v = v`
-- | - Composition: `(pure (<<<)) <*> f <*> g <*> h = f <*> (g <*> h)`
-- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
-- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
checkApplicativeShow
  ∷ ∀ f
  . Applicative f
  ⇒ Arbitrary (f A)
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f (B → C))
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Eq (f C)
  ⇒ Show (f A)
  ⇒ Show (f B)
  ⇒ Show (f C)
  ⇒ Proxy2 f
  → Effect Unit
checkApplicativeShow _ = do

  log "Checking 'Identity' law for Applicative"
  quickCheck' 1000 identity

  log "Checking 'Composition' law for Applicative"
  quickCheck' 1000 composition

  log "Checking 'Homomorphism' law for Applicative"
  quickCheck' 1000 homomorphism

  log "Checking 'Interchange' law for Applicative"
  quickCheck' 1000 interchange

  where

  identity ∷ f A → Result
  identity v = (pure F.identity <*> v) === v

  composition ∷ f (B → C) → f (A → B) → f A → Result
  composition f g x = (pure (<<<) <*> f <*> g <*> x) === (f <*> (g <*> x))

  homomorphism ∷ (A → B) → A → Result
  homomorphism f x = (pure f <*> pure x) === (pure (f x) ∷ f B)

  interchange ∷ A → f (A → B) → Result
  interchange y u = (u <*> pure y) === (pure (_ $ y) <*> u)
