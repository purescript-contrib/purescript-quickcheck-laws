module Test.QuickCheck.Laws.Control.Applicative where

import Prelude

import Control.Apply (lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy2)

-- | - Identity: `(pure id) <*> v = v`
-- | - Composition: `(pure (<<<)) <*> f <*> g <*> h = f <*> (g <*> h)`
-- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
-- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
checkApplicative
  ∷ ∀ eff f
  . Applicative f
  ⇒ Arbitrary (f A)
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f (B → C))
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Eq (f C)
  ⇒ Proxy2 f
  → QC eff Unit
checkApplicative _ =
  checkApplicativeGen
    (arbitrary :: Gen (f A))
    (arbitrary :: Gen (f (A → B)))
    (arbitrary :: Gen (f (B → C)))

checkApplicativeGen
  ∷ ∀ eff f
  . Applicative f
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Eq (f C)
  ⇒ Gen (f A)
  → Gen (f (A → B))
  → Gen (f (B → C))
  → QC eff Unit
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
  identity v = (pure id <*> v) == v

  composition ∷ f (B → C) → f (A → B) → f A → Boolean
  composition f g x = (pure (<<<) <*> f <*> g <*> x) == (f <*> (g <*> x))

  homomorphism ∷ (A → B) → A → Boolean
  homomorphism f x = (pure f <*> pure x) == (pure (f x) ∷ f B)

  interchange ∷ A → f (A → B) → Boolean
  interchange y u = (u <*> pure y) == (pure (_ $ y) <*> u)
