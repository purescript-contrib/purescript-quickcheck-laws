module Test.QuickCheck.Laws.Control.Bind where

import Prelude

import Control.Apply (lift3)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A)
import Type.Proxy (Proxy)

-- | - Associativity: `(x >>= f) >>= g = x >>= (\k → f k >>= g)`
checkBind
  ∷ ∀ m
  . Bind m
  ⇒ Arbitrary (m A)
  ⇒ Eq (m A)
  ⇒ Proxy m
  → Effect Unit
checkBind _ = checkBindGen (arbitrary :: Gen (m A)) (arbitrary :: Gen (A → m A))

checkBindGen
  ∷ ∀ m
  . Bind m
  ⇒ Eq (m A)
  ⇒ Gen (m A)
  → Gen (A → m A)
  → Effect Unit
checkBindGen gen genF = do
  log "Checking 'Associativity' law for Bind"
  quickCheck' 1000 $ lift3 associativity gen genF genF

  where

  associativity ∷ m A → (A → m A) → (A → m A) → Boolean
  associativity m f g = ((m >>= f) >>= g) == (m >>= (\x → f x >>= g))
