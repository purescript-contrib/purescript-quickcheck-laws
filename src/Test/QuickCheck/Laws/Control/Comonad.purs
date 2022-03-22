module Test.QuickCheck.Laws.Control.Comonad where

import Prelude

import Control.Apply (lift2)
import Control.Comonad (class Comonad, extract)
import Control.Extend ((<<=))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy)

-- | - Left Identity: `extract <<= x = x`
-- | - Right Identity: `extract (f <<= x) = f x`
checkComonad
  ∷ ∀ w
  . Comonad w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Eq (w A)
  ⇒ Proxy w
  → Effect Unit
checkComonad _ =
  checkComonadGen
    (arbitrary :: Gen (w A))
    (arbitrary :: Gen (w A → B))

checkComonadGen
  ∷ ∀ w
  . Comonad w
  ⇒ Eq (w A)
  ⇒ Gen (w A)
  → Gen (w A → B)
  → Effect Unit
checkComonadGen gen cogen = do
  log "Checking 'Left identity' law for Comonad"
  quickCheck' 1000 $ leftIdentity <$> gen

  log "Checking 'Right identity' law for Comonad"
  quickCheck' 1000 $ lift2 rightIdentity cogen gen

  where

  leftIdentity ∷ w A → Boolean
  leftIdentity x = (extract <<= x) == x

  rightIdentity ∷ (w A → B) → w A → Boolean
  rightIdentity f x = extract (f <<= x) == f x
