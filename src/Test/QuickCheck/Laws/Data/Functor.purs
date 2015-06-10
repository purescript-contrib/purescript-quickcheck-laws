module Test.QuickCheck.Laws.Data.Functor where

import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy2(), Proxy())

-- | - Identity: `(<$>) id = id`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
checkFunctor :: forall f. (Functor f, Arbitrary1 f, Eq1 f) => Proxy2 f -> QC Unit
checkFunctor _ = do

  log "Checking 'Identity' law for Functor"
  quickCheck identity

  log "Checking 'Composition' law for Functor"
  quickCheck composition

  where

  identity :: Wrap f A -> Boolean
  identity (Wrap f) = map id f `eq1` id f

  composition :: (A -> A) -> (A -> A) -> Wrap f A -> Boolean
  composition f g (Wrap x) = map (f <<< g) x `eq1` (map f <<< map g) x
