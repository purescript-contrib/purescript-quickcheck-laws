module Test.QuickCheck.Laws.Data.BooleanAlgebra where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.BooleanAlgebra (tt)

import Type.Proxy (Proxy)

import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Excluded middle: `a || not a = tt`
checkBooleanAlgebra
  ∷ ∀ a
  . Arbitrary a
  ⇒ BooleanAlgebra a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkBooleanAlgebra _ = do

  log "Checking 'Excluded middle' law for BooleanAlgebra"
  quickCheck' 1000 excludedMiddle

  where

  excludedMiddle ∷ a → Boolean
  excludedMiddle a = (a || not a) == tt
