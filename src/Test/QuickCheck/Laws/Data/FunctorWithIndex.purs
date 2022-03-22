module Test.QuickCheck.Laws.Data.FunctorWithIndex where

import Prelude

import Data.Function as F
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (class Coarbitrary, arbitrary, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy)

-- | - Identity: `mapWithIndex (\_ a → a) = identity`
-- | - Composition: `mapWithIndex f . mapWithIndex g = mapWithIndex (\i → f i <<< g i)`
checkFunctorWithIndex
  ∷ ∀ f i
  . FunctorWithIndex i f
  ⇒ Arbitrary (f A)
  ⇒ Coarbitrary i
  ⇒ Eq (f A)
  ⇒ Proxy f
  → Effect Unit
checkFunctorWithIndex _ = checkFunctorWithIndexGen (arbitrary :: Gen (f A))

checkFunctorWithIndexGen
  ∷ ∀ f i
  . FunctorWithIndex i f
  ⇒ Coarbitrary i
  ⇒ Eq (f A)
  ⇒ Gen (f A)
  → Effect Unit
checkFunctorWithIndexGen gen = do

  log "Checking 'Identity' law for FunctorWithIndex"
  quickCheck' 1000 $ identity <$> gen

  log "Checking 'Composition' law for FunctorWithIndex"
  quickCheck' 1000 $ composition <$> gen

  where

  identity ∷ f A → Boolean
  identity x = mapWithIndex (\_ a → a) x == F.identity x

  composition ∷ f A → (i → B → A) → (i → A → B) → Boolean
  composition fa f g =
    (mapWithIndex f <<< mapWithIndex g) fa
      == mapWithIndex (\i -> f i <<< g i) fa
