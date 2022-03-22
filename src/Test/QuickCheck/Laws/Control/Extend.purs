module Test.QuickCheck.Laws.Control.Extend where

import Prelude

import Control.Apply (lift3)
import Control.Extend (class Extend, (<<=))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy)

-- | - Associativity: `extend f <<< extend g = extend (f <<< extend g)`
checkExtend
  ∷ ∀ w
  . Extend w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Coarbitrary (w B)
  ⇒ Eq (w C)
  ⇒ Proxy w
  → Effect Unit
checkExtend _ =
  checkExtendGen
    (arbitrary :: Gen (w A))
    (arbitrary :: Gen (w B → C))
    (arbitrary :: Gen (w A → B))

checkExtendGen
  ∷ ∀ w
  . Extend w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Coarbitrary (w B)
  ⇒ Eq (w C)
  ⇒ Gen (w A)
  → Gen (w B → C)
  → Gen (w A → B)
  → Effect Unit
checkExtendGen gen genwbc genwab = do
  log "Checking 'Associativity' law for Extend"
  quickCheck' 1000 $ lift3 associativity genwbc genwab gen

  where

  associativity ∷ (w B → C) → (w A → B) → w A → Boolean
  associativity f g x =
    ((f <<= _) <<< (g <<= _) $ x) == (f <<< (g <<= _) <<= x)
