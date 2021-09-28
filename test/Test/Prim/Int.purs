module Test.Prim.Int where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkInt ∷ Effect Unit
checkInt = checkLaws "Int" do
  Data.checkEq prxInt
  Data.checkOrd prxInt
  Data.checkCommutativeRing prxInt
  Data.checkSemiring prxInt
  -- Necessary for the EuclideanRing test
  -- so as to prevent integer overflow when multiplying large integer values
  Data.checkEuclideanRingGen (chooseInt (-10_000) 10_000)
  Data.checkRing prxInt
  where
  prxInt = Proxy ∷ Proxy Int
