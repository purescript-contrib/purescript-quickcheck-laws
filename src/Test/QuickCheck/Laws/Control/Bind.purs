module Test.QuickCheck.Laws.Control.Bind where

import Prelude

import Control.Apply (lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, arbitrary, QC, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A)
import Type.Proxy (Proxy2)

-- | - Associativity: `(x >>= f) >>= g = x >>= (\k → f k >>= g)`
checkBind
  ∷ ∀ eff m
  . Bind m
  ⇒ Arbitrary (m A)
  ⇒ Eq (m A)
  ⇒ Proxy2 m
  → QC eff Unit
checkBind _ = checkBindGen (arbitrary :: Gen (m A)) (arbitrary :: Gen (A → m A))

checkBindGen
  ∷ ∀ eff m
  . Bind m
  ⇒ Eq (m A)
  ⇒ Gen (m A)
  → Gen (A → m A)
  → QC eff Unit
checkBindGen gen genF = do

  log "Checking 'Associativity' law for Bind"
  quickCheck' 1000 $ lift3 associativity gen genF genF

  where

  associativity ∷ m A → (A → m A) → (A → m A) → Boolean
  associativity m f g = ((m >>= f) >>= g) == (m >>= (\x → f x >>= g))
