module Test.QuickCheck.Laws.Control.Monad where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy2())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A())

-- | - Left Identity: `pure x >>= f = f x`
-- | - Right Identity: `x >>= pure = x`
checkMonad :: forall eff m. (Monad m, Arbitrary (m A), Eq (m A)) => Proxy2 m -> QC eff Unit
checkMonad _ = do

  log "Checking 'Left identity' law for Monad"
  quickCheck' 1000 leftIdentity

  log "Checking 'Right identity' law for Monad"
  quickCheck' 1000 rightIdentity

  where

  leftIdentity :: A -> (A -> m A) -> Boolean
  leftIdentity x f = (pure x >>= f) == f x

  rightIdentity :: m A -> Boolean
  rightIdentity m = (m >>= pure) == m
