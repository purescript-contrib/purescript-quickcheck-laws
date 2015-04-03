module Test.QuickCheck.Laws.Monad where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

checkMonad :: forall m a. (Monad m,
                           Arbitrary a,
                           Arbitrary (m a),
                           CoArbitrary a,
                           Eq (m a)) => m a -> QC Unit
checkMonad _ = do

  trace "Checking 'Left identity' law for Monad"
  quickCheck leftIdentity

  trace "Checking 'Right identity' law for Monad"
  quickCheck rightIdentity

  where

  leftIdentity :: a -> (a -> m a) -> Boolean
  leftIdentity x f = (pure x >>= f) == f x

  rightIdentity :: m a -> Boolean
  rightIdentity m = (m >>= pure) == m
