module Test.QuickCheck.Laws.Data.Functor where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Type.Proxy (Proxy2)

import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)

-- | - Identity: `(<$>) identity = identity`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
checkFunctor
  ∷ ∀ f
  . Functor f
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Proxy2 f
  → Effect Unit
checkFunctor _ = do

  log "Checking 'Identity' law for Functor"
  quickCheck' 1000 identity'

  log "Checking 'Composition' law for Functor"
  quickCheck' 1000 composition

  where

  identity' ∷ f A → Boolean
  identity' f = (identity <$> f) == identity f

  composition ∷ (B → A) → (A → B) → f A → Boolean
  composition f g x = ((<$>) (f <<< g) x) == (((f <$> _) <<< (g <$> _)) x)
