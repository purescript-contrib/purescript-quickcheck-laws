module Test.Prim.Number (checkNumber) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Data.ApproxNumber (ApproxNumber())
import Test.QuickCheck.Laws.Data.CommutativeRing (checkCommutativeRing)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.EuclideanRing (checkEuclideanRing)
import Test.QuickCheck.Laws.Data.Field (checkField)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Ring (checkRing)
import Test.QuickCheck.Laws.Data.Semiring (checkSemiring)
import Type.Proxy (Proxy(..))

import Prelude

prxNumber :: Proxy ApproxNumber
prxNumber = Proxy

checkNumber = do
  log "\n\nChecking Number instances...\n"
  checkEq prxNumber
  checkOrd prxNumber
  checkSemiring prxNumber
  checkEuclideanRing prxNumber
  checkRing prxNumber
  checkField prxNumber
  checkCommutativeRing prxNumber
