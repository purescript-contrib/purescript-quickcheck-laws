module Test.QuickCheck.Laws.Data.FunctorWithIndex where

import Prelude

import Data.Function as F
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (class Coarbitrary, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Identity: `mapWithIndex (\_ a → a) = identity`
-- | - Composition: `mapWithIndex f . mapWithIndex g = mapWithIndex (\i → f i <<< g i)`
checkFunctorWithIndex
  ∷ ∀ f i
  . FunctorWithIndex i f
  ⇒ Arbitrary (f A)
  ⇒ Coarbitrary i
  ⇒ Eq (f A)
  ⇒ Proxy2 f
  → Effect Unit
checkFunctorWithIndex _ = do

  log "Checking 'Identity' law for FunctorWithIndex"
  quickCheck' 1000 identity

  log "Checking 'Composition' law for FunctorWithIndex"
  quickCheck' 1000 composition

  where

  identity ∷ f A → Boolean
  identity x = mapWithIndex (\_ a → a) x == F.identity x

  composition ∷ (i → B → A) → (i → A → B) → f A → Boolean
  composition f g x =
    (mapWithIndex f <<< mapWithIndex g) x
      == mapWithIndex (\i -> f i <<< g i) x