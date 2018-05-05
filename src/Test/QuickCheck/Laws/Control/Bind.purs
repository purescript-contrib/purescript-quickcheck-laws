module Test.QuickCheck.Laws.Control.Bind where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A)
import Type.Proxy (Proxy2)

-- | - Associativity: `(x >>= f) >>= g = x >>= (\k → f k >>= g)`
checkBind
  ∷ ∀ m
  . Bind m
  ⇒ Arbitrary (m A)
  ⇒ Eq (m A)
  ⇒ Proxy2 m
  → Effect Unit
checkBind _ = do

  log "Checking 'Associativity' law for Bind"
  quickCheck' 1000 associativity

  where

  associativity ∷ m A → (A → m A) → (A → m A) → Boolean
  associativity m f g = ((m >>= f) >>= g) == (m >>= (\x → f x >>= g))
