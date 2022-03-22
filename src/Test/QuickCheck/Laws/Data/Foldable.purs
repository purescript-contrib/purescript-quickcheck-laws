module Test.QuickCheck.Laws.Data.Foldable where

import Prelude

import Data.Foldable (foldMap, fold, foldlDefault, foldl, foldr, class Foldable, foldrDefault)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy)


-- | - foldr: `foldr = foldrDefault`
-- | - foldl: `foldl = foldlDefault`
checkFoldable
  ∷ ∀ f
  . Foldable f
  ⇒ Arbitrary (f A)
  ⇒ Proxy f
  → Effect Unit
checkFoldable _ = checkFoldableGen (arbitrary :: Gen (f A))

checkFoldableGen
  ∷ ∀ f
  . Foldable f
  ⇒ Gen (f A)
  → Effect Unit
checkFoldableGen gen = do
  log "Checking 'foldr' law for Foldable"
  quickCheck' 1000 $ foldrLaw <$> gen

  log "Checking 'foldl' law for Foldable"
  quickCheck' 1000 $ foldlLaw <$> gen

  where
    foldrLaw :: f A -> (A -> B -> B) -> B -> Boolean
    foldrLaw fa f b = foldr f b fa == foldrDefault f b fa

    foldlLaw :: f A -> (B -> A -> B) -> B -> Boolean
    foldlLaw fa f b = foldl f b fa == foldlDefault f b fa

-- | foldMap: `foldMap = fold <<< map`
checkFoldableFunctor
  ∷ ∀ f
  . Foldable f
  ⇒ Functor f
  ⇒ Arbitrary (f A)
  ⇒ Proxy f
  → Effect Unit
checkFoldableFunctor _ = checkFoldableFunctorGen (arbitrary :: Gen (f A))

checkFoldableFunctorGen
  ∷ ∀ f
  . Foldable f
  ⇒ Functor f
  ⇒ Gen (f A)
  → Effect Unit
checkFoldableFunctorGen gen = do
  checkFoldableGen gen

  log "Checking 'foldMap' law for Foldable"
  quickCheck' 1000 $ flip foldMapLaw <$> gen

  where
    foldMapLaw :: (A -> B) -> f A -> Boolean
    foldMapLaw f t = foldMap f t == (fold <<< map f) t
