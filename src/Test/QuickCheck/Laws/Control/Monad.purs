module Test.QuickCheck.Laws.Control.Monad where

import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Left Identity: `pure x >>= f = f x`
-- | - Right Identity: `x >>= pure = x`
checkMonad :: forall m. (Monad m, Arbitrary1 m, Eq1 m) => Proxy2 m -> QC Unit
checkMonad _ = do

  log "Checking 'Left identity' law for Monad"
  quickCheck leftIdentity

  log "Checking 'Right identity' law for Monad"
  quickCheck rightIdentity

  where

  leftIdentity :: A -> (A -> Wrap m B) -> Boolean
  leftIdentity x f = (pure x >>= f') `eq1` f' x
    where
    f' = f >>> unwrap

  rightIdentity :: Wrap m A -> Boolean
  rightIdentity (Wrap m) = (m >>= pure) `eq1` m
