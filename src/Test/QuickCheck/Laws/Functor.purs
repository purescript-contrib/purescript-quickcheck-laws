module Test.QuickCheck.Laws.Functor where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

checkFunctor :: forall f a b c. (Functor f,
                                 Arbitrary a,
                                 Arbitrary (f a),
                                 Arbitrary (f c),
                                 Arbitrary (a -> b),
                                 Arbitrary (b -> c),
                                 Eq (f c)) => f a -> f b -> f c -> QC Unit
checkFunctor _ _ _ = do
  trace "Checking 'Identity' law for Functor"
  quickCheck identity

  trace "Checking 'Composition' law for Functor"
  quickCheck composition

  where

  identity :: f c -> Boolean
  identity f = (id <$> f) == id f

  composition :: (b -> c) -> (a -> b) -> f a -> Boolean
  composition f g x = ((<$>) (f <<< g) x) == (((f <$>) <<< (g <$>)) x)
