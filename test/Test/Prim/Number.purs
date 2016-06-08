module Test.Prim.Number (checkNumber) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Data.ApproxNumber (ApproxNumber())
import Test.QuickCheck.Laws.Data.CommutativeRing
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.EuclideanRing
import Test.QuickCheck.Laws.Data.Field
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Ring
import Test.QuickCheck.Laws.Data.Semiring
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
