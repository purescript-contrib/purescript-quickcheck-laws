module Test.Data.Unit where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkUnit ∷ Effect Unit
checkUnit = checkLaws "Unit" do
  Data.checkEq prxUnit
  Data.checkOrd prxUnit
  Data.checkBounded prxUnit
  Data.checkEnum prxUnit
  Data.checkBoundedEnum prxUnit
  Data.checkSemigroup prxUnit
  Data.checkMonoid prxUnit
  Data.checkSemiring prxUnit
  Data.checkRing prxUnit
  Data.checkCommutativeRing prxUnit
  Data.checkHeytingAlgebra prxUnit
  Data.checkBooleanAlgebra prxUnit
  where
  prxUnit = Proxy ∷ Proxy Unit
