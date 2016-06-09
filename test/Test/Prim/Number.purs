module Test.Prim.Number where

import Prelude

import Test.QuickCheck.Data.ApproxNumber (ApproxNumber)
import Test.QuickCheck.Laws (QC, checkLaws)
import Test.QuickCheck.Laws.Data as Data

import Type.Proxy (Proxy(..))

checkNumber ∷ ∀ eff. QC eff Unit
checkNumber = checkLaws "Number" do
  Data.checkEq prxNumber
  Data.checkOrd prxNumber
  Data.checkSemiring prxNumber
  Data.checkEuclideanRing prxNumber
  Data.checkRing prxNumber
  Data.checkField prxNumber
  Data.checkCommutativeRing prxNumber
  where
  prxNumber = Proxy ∷ Proxy ApproxNumber
