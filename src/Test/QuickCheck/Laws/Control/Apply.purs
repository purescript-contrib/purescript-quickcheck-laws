module Test.QuickCheck.Laws.Control.Apply where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
checkApply :: forall f. (Apply f, Arbitrary1 f, Eq1 f) => Proxy2 f -> QC Unit
checkApply _ = do

  log "Checking 'Associative composition' law for Apply"
  quickCheck associativeComposition

  where

  associativeComposition :: Wrap f (B -> C) -> Wrap f (A -> B) -> Wrap f A -> Boolean
  associativeComposition (Wrap f) (Wrap g) (Wrap x) = (compose <$> f <*> g <*> x) `eq1` (f <*> (g <*> x))
