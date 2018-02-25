module Test.QuickCheck.Laws.Data.Monoid where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Monoid (class Monoid, mempty)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Left identity: `mempty <> x = x`
-- | - Right identity: `x <> mempty = x`
checkMonoid
  ∷ ∀ eff m
  . Monoid m
  ⇒ Arbitrary m
  ⇒ Eq m
  ⇒ Proxy m
  → QC eff Unit
checkMonoid _ = checkMonoidGen (arbitrary :: Gen m)

checkMonoidGen
  ∷ ∀ eff m
  . Monoid m
  ⇒ Eq m
  ⇒ Gen m
  → QC eff Unit
checkMonoidGen gen = do

  log "Checking 'Left identity' law for Monoid"
  quickCheck' 1000 $ leftIdentity <$> gen

  log "Checking 'Right identity' law for Monoid"
  quickCheck' 1000 $ rightIdentity <$> gen

  where

  leftIdentity ∷ m → Boolean
  leftIdentity x = mempty <> x == x

  rightIdentity ∷ m → Boolean
  rightIdentity x = x <> mempty == x
