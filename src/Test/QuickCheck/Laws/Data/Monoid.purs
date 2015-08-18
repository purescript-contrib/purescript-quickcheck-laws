module Test.QuickCheck.Laws.Data.Monoid where

import Control.Monad.Eff.Console (log)
import Data.Monoid (Monoid, mempty)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

import Prelude

-- | - Left identity: `mempty <> x = x`
-- | - Right identity: `x <> mempty = x`
checkMonoid :: forall m eff. (Monoid m,
                              Arbitrary m,
                              Eq m) => Proxy m
                                    -> QC eff Unit
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
