module Test.QuickCheck.Laws.Apply where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

checkApply :: forall f a b c. (Apply f,
                               Arbitrary (f a),
                               Arbitrary (f (a -> b)),
                               Arbitrary (f (b -> c)),
                               Eq (f c)) => f a -> f b -> f c -> QC Unit
checkApply _ _ _ = do

  trace "Checking 'Associative composition' law for Apply"
  quickCheck associativeComposition

  where

  associativeComposition :: f (b -> c) -> f (a -> b) -> f a -> Boolean
  associativeComposition f g x = ((<<<) <$> f <*> g <*> x) == (f <*> (g <*> x))
