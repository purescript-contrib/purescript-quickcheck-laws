module Test.QuickCheck.Laws.Data.Foldable where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Foldable (foldMap, fold, foldlDefault, foldl, foldr, class Foldable, foldrDefault)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)


-- | - foldr: `foldr = foldrDefault`
-- | - foldl: `foldl = foldlDefault`
checkFoldable
  ∷ ∀ eff f
  . Foldable f
  ⇒ Arbitrary (f A)
  ⇒ Proxy2 f
  → QC eff Unit
checkFoldable _ = checkFoldableGen (arbitrary :: Gen (f A))

checkFoldableGen
  ∷ ∀ eff f
  . Foldable f
  ⇒ Gen (f A)
  → QC eff Unit
checkFoldableGen gen = do

  log "Checking 'foldr' law for Foldable"
  quickCheck' 1000 $ foldrLaw <$> gen

  log "Checking 'foldl' law for Foldable"
  quickCheck' 1000 $ foldlLaw <$> gen

  where
    foldrLaw :: f A -> (A -> B -> B) -> B -> Boolean
    foldrLaw t f z = foldr f z t == foldrDefault f z t

    foldlLaw :: f A -> (B -> A -> B) -> B -> Boolean
    foldlLaw t f z = foldl f z t == foldlDefault f z t
                                                           

-- | foldMap: `foldMap = fold <<< map`
checkFoldableFunctor
  ∷ ∀ eff f
  . Foldable f
  ⇒ Functor f
  ⇒ Arbitrary (f A)
  ⇒ Proxy2 f
  → QC eff Unit
checkFoldableFunctor _ = checkFoldableFunctorGen (arbitrary :: Gen (f A))

checkFoldableFunctorGen
  ∷ ∀ eff f
  . Foldable f
  ⇒ Functor f
  ⇒ Gen (f A)
  → QC eff Unit
checkFoldableFunctorGen gen = do

  checkFoldableGen gen

  log "Checking 'foldMap' law for Foldable"
  quickCheck' 1000 $ flip foldMapLaw <$> gen

  where
    foldMapLaw :: (A -> B) -> f A -> Boolean
    foldMapLaw f t = foldMap f t == (fold <<< map f) t
