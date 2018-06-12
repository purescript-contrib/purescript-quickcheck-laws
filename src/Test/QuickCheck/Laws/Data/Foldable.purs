module Test.QuickCheck.Laws.Data.Foldable where

import Prelude

import Data.Foldable (foldMap, fold, foldlDefault, foldl, foldr, class Foldable, foldrDefault)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)


-- | - foldr: `foldr = foldrDefault`
-- | - foldl: `foldl = foldlDefault`
checkFoldable
  ∷ ∀ f
  . Foldable f
  ⇒ Arbitrary (f A)
  ⇒ Proxy2 f
  → Effect Unit
checkFoldable _ = do

  log "Checking 'foldr' law for Foldable"
  quickCheck' 1000 foldrLaw

  log "Checking 'foldl' law for Foldable"
  quickCheck' 1000 foldlLaw

  where
    foldrLaw :: (A -> B -> B) -> B -> f A -> Boolean
    foldrLaw f z t = foldr f z t == foldrDefault f z t

    foldlLaw :: (B -> A -> B) -> B -> f A -> Boolean
    foldlLaw f z t = foldl f z t == foldlDefault f z t


-- | foldMap: `foldMap = fold <<< map`
checkFoldableFunctor
  ∷ ∀ f
  . Foldable f
  ⇒ Functor f
  ⇒ Arbitrary (f A)
  ⇒ Proxy2 f
  → Effect Unit
checkFoldableFunctor ff = do

  checkFoldable ff

  log "Checking 'foldMap' law for Foldable"
  quickCheck' 1000 foldMapLaw

  where
    foldMapLaw :: (A -> B) -> f A -> Boolean
    foldMapLaw f t = foldMap f t == (fold <<< map f) t
