module Test.QuickCheck.Laws.Control.Monad where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

import Prelude

-- | - Left Identity: `pure x >>= f = f x`
-- | - Right Identity: `x >>= pure = x`
checkMonad :: forall m a. (Monad m,
                           Arbitrary a,
                           Arbitrary (m a),
                           Coarbitrary a,
                           Eq (m a)) => Proxy2 m
                                     -> Proxy a
                                     -> QC Unit
checkMonad _ _ = do

  log "Checking 'Left identity' law for Monad"
  quickCheck leftIdentity

  log "Checking 'Right identity' law for Monad"
  quickCheck rightIdentity

  where

  leftIdentity :: a -> (a -> m a) -> Boolean
  leftIdentity x f = (pure x >>= f) == f x

  rightIdentity :: m a -> Boolean
  rightIdentity m = (m >>= pure) == m
