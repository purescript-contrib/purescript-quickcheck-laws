module Test.QuickCheck.Laws.Data.Functor where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)
import Type.Proxy (Proxy2(), Proxy())

-- | - Identity: `(<$>) id = id`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
checkFunctor :: forall f a b. (Functor f, Arbitrary (f a), Arbitrary (a -> b), Arbitrary (b -> a), Eq (f a)) => Proxy2 f -> Proxy a -> Proxy b -> QC Unit
checkFunctor _ _ _ = do

  trace "Checking 'Identity' law for Functor"
  quickCheck identity

  trace "Checking 'Composition' law for Functor"
  quickCheck composition

  where

  identity :: f a -> Boolean
  identity f = (id <$> f) == id f

  composition :: (b -> a) -> (a -> b) -> f a -> Boolean
  composition f g x = ((<$>) (f <<< g) x) == (((f <$>) <<< (g <$>)) x)
