module Test.QuickCheck.Laws.Data.Foldable where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Foldable (foldMap, fold, foldlDefault, foldl, foldr, class Foldable, foldrDefault)
import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)


-- | - foldr: `foldr = foldrDefault`
-- | - foldl: `foldl = foldlDefault`
checkFoldable
  ∷ ∀ eff f
  . (Foldable f, Arbitrary (f A))
  ⇒ Proxy2 f
  → QC eff Unit
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
  ∷ ∀ eff f
  . (Foldable f, Functor f, Arbitrary (f A))
  ⇒ Proxy2 f
  → QC eff Unit
checkFoldableFunctor ff = do

  checkFoldable ff

  log "Checking 'foldMap' law for Foldable"
  quickCheck' 1000 foldMapLaw

  where
    foldMapLaw :: (A -> B) -> f A -> Boolean
    foldMapLaw f t = foldMap f t == (fold <<< map f) t
