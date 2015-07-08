module Test.QuickCheck.Laws.Data.Functor where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy2(), Proxy())

import Prelude

-- | - Identity: `(<$>) id = id`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
checkFunctor :: forall f a b. (Functor f,
                               Arbitrary a,
                               Arbitrary b,
                               Arbitrary (f a),
                               Coarbitrary a,
                               Coarbitrary b,
                               Eq (f a)) => Proxy2 f
                                         -> Proxy a
                                         -> Proxy b
                                         -> QC Unit
checkFunctor _ _ _ = do

  log "Checking 'Identity' law for Functor"
  quickCheck identity

  log "Checking 'Composition' law for Functor"
  quickCheck composition

  where

  identity :: f a -> Boolean
  identity f = (id <$> f) == id f

  composition :: (b -> a) -> (a -> b) -> f a -> Boolean
  composition f g x = ((<$>) (f <<< g) x) == (((f <$>) <<< (g <$>)) x)
