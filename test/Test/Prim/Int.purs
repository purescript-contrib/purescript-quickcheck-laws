module Test.Prim.Int where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkInt ∷ Effect Unit
checkInt = checkLaws "Int" do
  Data.checkEq prxInt
  Data.checkOrd prxInt
  Data.checkCommutativeRing prxInt
  Data.checkSemiring prxInt
  Data.checkEuclideanRing prxInt
  Data.checkRing prxInt
  Data.checkEqShow prxInt
  Data.checkOrdShow prxInt
  Data.checkCommutativeRingShow prxInt
  Data.checkSemiringShow prxInt
  Data.checkEuclideanRingShow prxInt
  Data.checkRingShow prxInt
  where
  prxInt = Proxy ∷ Proxy Int
