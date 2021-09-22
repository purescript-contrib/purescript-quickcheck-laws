module Test.QuickCheck.Laws.Data.Monoid where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Left identity: `mempty <> x = x`
-- | - Right identity: `x <> mempty = x`
checkMonoid
  ∷ ∀ m
  . Monoid m
  ⇒ Arbitrary m
  ⇒ Eq m
  ⇒ Proxy m
  → Effect Unit
checkMonoid _ = checkMonoidGen (arbitrary :: Gen m)

checkMonoidGen
  ∷ ∀ m
  . Monoid m
  ⇒ Eq m
  ⇒ Gen m
  → Effect Unit
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
