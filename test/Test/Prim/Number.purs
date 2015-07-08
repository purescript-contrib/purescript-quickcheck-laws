module Test.Prim.Number (checkNumber) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Data.ApproxNumber (ApproxNumber())
import Test.QuickCheck.Laws.Data.DivisionRing
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.ModuloSemiring
import Test.QuickCheck.Laws.Data.Num
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
  checkModuloSemiring prxNumber
  checkRing prxNumber
  checkDivisionRing prxNumber
  checkNum prxNumber
