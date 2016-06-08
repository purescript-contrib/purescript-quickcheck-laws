module Test.Data.Int (checkInt) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.CommutativeRing (checkCommutativeRing)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.EuclideanRing (checkEuclideanRing)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Ring (checkRing)
import Test.QuickCheck.Laws.Data.Semiring (checkSemiring)
import Type.Proxy (Proxy(..))

import Prelude

prxInt :: Proxy Int
prxInt = Proxy

checkInt = do
  log "\n\nChecking Int instances...\n"
  checkEq prxInt
  checkOrd prxInt
  checkCommutativeRing prxInt
  checkSemiring prxInt
  checkEuclideanRing prxInt
  checkRing prxInt
