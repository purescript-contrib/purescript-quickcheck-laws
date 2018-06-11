module Test.QuickCheck.Laws.Control.Extend where

import Prelude

import Control.Extend (class Extend, (<<=))
import Effect (Effect)
import Effect.Console (log)

import Type.Proxy (Proxy2)

import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary)
import Test.QuickCheck.Laws (A, B, C)

-- | - Associativity: `extend f <<< extend g = extend (f <<< extend g)`
checkExtend
  ∷ ∀ w
  . Extend w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Coarbitrary (w B)
  ⇒ Eq (w C)
  ⇒ Proxy2 w
  → Effect Unit
checkExtend _ = do

  log "Checking 'Associativity' law for Extend"
  quickCheck' 1000 associativity

  where

  associativity ∷ (w B → C) → (w A → B) → w A → Boolean
  associativity f g x =
    ((f <<= _) <<< (g <<= _) $ x) == (f <<< (g <<= _) <<= x)
