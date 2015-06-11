module Test.QuickCheck.Laws.Control.Apply where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

import Prelude

-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
checkApply :: forall f a b c. (Apply f,
                               Arbitrary (f a),
                               Arbitrary (f (a -> b)),
                               Arbitrary (f (b -> c)),
                               Eq (f c)) => Proxy2 f
                                         -> Proxy a
                                         -> Proxy b
                                         -> Proxy c
                                         -> QC Unit
checkApply _ _ _ _ = do

  log "Checking 'Associative composition' law for Apply"
  quickCheck associativeComposition

  where

  associativeComposition :: f (b -> c) -> f (a -> b) -> f a -> Boolean
  associativeComposition f g x = ((<<<) <$> f <*> g <*> x) == (f <*> (g <*> x))
