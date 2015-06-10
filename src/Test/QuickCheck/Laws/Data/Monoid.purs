module Test.QuickCheck.Laws.Data.Monoid where

import Prelude

import Data.Monoid (Monoid, mempty)
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)

import Type.Proxy (Proxy())

-- | - Left identity: `mempty <> x = x`
-- | - Right identity: `x <> mempty = x`
checkMonoid :: forall m. (Monoid m,
                          Arbitrary m,
                          Eq m) => Proxy m
                                -> QC Unit
checkMonoid _ = do

  log "Checking 'Left identity' law for Monoid"
  quickCheck leftIdentity

  log "Checking 'Right identity' law for Monoid"
  quickCheck rightIdentity

  where

  leftIdentity :: m -> Boolean
  leftIdentity x = mempty <> x == x

  rightIdentity :: m -> Boolean
  rightIdentity x = x <> mempty == x
