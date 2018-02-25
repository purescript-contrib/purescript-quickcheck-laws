module Test.QuickCheck.Laws.Data.Bounded where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Ordering: `bottom <= a <= top`
checkBounded
  ∷ ∀ eff a
  . Arbitrary a
  ⇒ Bounded a
  ⇒ Ord a
  ⇒ Proxy a
  → QC eff Unit
checkBounded _ = checkBoundedGen (arbitrary :: Gen a)

checkBoundedGen
  ∷ ∀ eff a
  . Bounded a
  ⇒ Ord a
  ⇒ Gen a
  → QC eff Unit
checkBoundedGen gen = do

  log "Checking 'Ordering' law for Bounded"
  quickCheck' 1000 $ ordering <$> gen

  where

  ordering ∷ a → Boolean
  ordering a = bottom <= a && a <= top
