module Test.QuickCheck.Laws.Data.Functor where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Identity: `(<$>) id = id`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
checkFunctor
  ∷ ∀ eff f
  . Functor f
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Proxy2 f
  → QC eff Unit
checkFunctor _ = checkFunctorGen (arbitrary :: Gen (f A))

checkFunctorGen
  ∷ ∀ eff f
  . Functor f
  ⇒ Eq (f A)
  ⇒ Gen (f A)
  → QC eff Unit
checkFunctorGen gen = do

  log "Checking 'Identity' law for Functor"
  quickCheck' 1000 $ identity <$> gen

  log "Checking 'Composition' law for Functor"
  quickCheck' 1000 $ composition <$> gen

  where

  identity ∷ f A → Boolean
  identity f = (id <$> f) == id f

  composition ∷ f A → (B → A) → (A → B) → Boolean
  composition x f g = ((<$>) (f <<< g) x) == (((f <$> _) <<< (g <$> _)) x)
