module Test.QuickCheck.Laws.Data.Functor where

import Prelude

import Data.Function as F
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Identity: `(<$>) id = id`
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
  quickCheck' 1000 identity

  log "Checking 'Composition' law for Functor"
  quickCheck' 1000 composition

  where

  identity ∷ f A → Boolean
  identity f = (F.identity <$> f) == F.identity f

  composition ∷ (B → A) → (A → B) → f A → Boolean
  composition f g x = ((<$>) (f <<< g) x) == (((f <$> _) <<< (g <$> _)) x)


-- | Like `checkFunctor`, but with better error reporting.
-- | - Identity: `(<$>) id = id`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
checkFunctorShow
  ∷ ∀ f
  . Functor f
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Show (f A)
  ⇒ Proxy2 f
  → Effect Unit
checkFunctorShow _ = do

  log "Checking 'Identity' law for Functor"
  quickCheck' 1000 identity

  log "Checking 'Composition' law for Functor"
  quickCheck' 1000 composition

  where

  identity ∷ f A → Result
  identity f = (F.identity <$> f) === F.identity f

  composition ∷ (B → A) → (A → B) → f A → Result
  composition f g x = ((<$>) (f <<< g) x) === (((f <$> _) <<< (g <$> _)) x)
