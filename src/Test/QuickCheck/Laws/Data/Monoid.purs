module Test.QuickCheck.Laws.Data.Monoid where

import Prelude

import Control.Monad.Eff.Console (log)

import Data.Monoid (class Monoid, mempty)

import Type.Proxy (Proxy)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Left identity: `mempty <> x = x`
-- | - Right identity: `x <> mempty = x`
checkMonoid
  ∷ ∀ eff m
  . (Monoid m, Arbitrary m, Eq m)
  ⇒ Proxy m
  → QC eff Unit
checkMonoid _ = do

  log "Checking 'Left identity' law for Monoid"
  quickCheck' 1000 leftIdentity

  log "Checking 'Right identity' law for Monoid"
  quickCheck' 1000 rightIdentity

  where

  leftIdentity ∷ m → Boolean
  leftIdentity x = mempty <> x == x

  rightIdentity ∷ m → Boolean
  rightIdentity x = x <> mempty == x
