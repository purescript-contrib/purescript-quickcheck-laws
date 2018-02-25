module Test.QuickCheck.Laws.Control.Extend where

import Prelude

import Control.Apply (lift3)
import Control.Extend (class Extend, (<<=))
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy2)

-- | - Associativity: `extend f <<< extend g = extend (f <<< extend g)`
checkExtend
  ∷ ∀ eff w
  . Extend w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Coarbitrary (w B)
  ⇒ Eq (w C)
  ⇒ Proxy2 w
  → QC eff Unit
checkExtend _ =
  checkExtendGen
    (arbitrary :: Gen (w A))
    (arbitrary :: Gen (w B → C))
    (arbitrary :: Gen (w A → B))

checkExtendGen
  ∷ ∀ eff w
  . Extend w
  ⇒ Arbitrary (w A)
  ⇒ Coarbitrary (w A)
  ⇒ Coarbitrary (w B)
  ⇒ Eq (w C)
  ⇒ Gen (w A)
  → Gen (w B → C)
  → Gen (w A → B)
  → QC eff Unit
checkExtendGen gen genwbc genwab = do

  log "Checking 'Associativity' law for Extend"
  quickCheck' 1000 $ lift3 associativity genwbc genwab gen

  where

  associativity ∷ (w B → C) → (w A → B) → w A → Boolean
  associativity f g x =
    ((f <<= _) <<< (g <<= _) $ x) == (f <<< (g <<= _) <<= x)
