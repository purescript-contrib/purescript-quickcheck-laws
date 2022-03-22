module Test.QuickCheck.Laws.Control.Applicative where

import Prelude

import Control.Apply (lift3)
import Data.Function as F
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy)

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
  ⇒ Proxy f
  → Effect Unit
checkApplicative _ =
  checkApplicativeGen
    (arbitrary :: Gen (f A))
    (arbitrary :: Gen (f (A → B)))
    (arbitrary :: Gen (f (B → C)))

checkApplicativeGen
  ∷ ∀ f
  . Applicative f
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Eq (f C)
  ⇒ Gen (f A)
  → Gen (f (A → B))
  → Gen (f (B → C))
  → Effect Unit
checkApplicativeGen gen genab genbc = do
  log "Checking 'Identity' law for Applicative"
  quickCheck' 1000 $ identity <$> gen

  log "Checking 'Composition' law for Applicative"
  quickCheck' 1000 $ lift3 composition genbc genab gen

  log "Checking 'Homomorphism' law for Applicative"
  quickCheck' 1000 homomorphism

  log "Checking 'Interchange' law for Applicative"
  quickCheck' 1000 $ flip interchange <$> genab

  where

  identity ∷ f A → Boolean
  identity v = (pure F.identity <*> v) == v

  composition ∷ f (B → C) → f (A → B) → f A → Boolean
  composition f g x = (pure (<<<) <*> f <*> g <*> x) == (f <*> (g <*> x))

  homomorphism ∷ (A → B) → A → Boolean
  homomorphism f x = (pure f <*> pure x) == (pure (f x) ∷ f B)

  interchange ∷ A → f (A → B) → Boolean
  interchange y u = (u <*> pure y) == (pure (_ $ y) <*> u)
