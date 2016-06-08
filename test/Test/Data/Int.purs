module Test.Data.Int (checkInt) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.CommutativeRing
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.EuclideanRing
import Test.QuickCheck.Laws.Data.Field
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Ring
import Test.QuickCheck.Laws.Data.Semiring
import Type.Proxy (Proxy(..))

import Prelude

prxInt :: Proxy Int
prxInt = Proxy

checkInt = do
  log "\n\nChecking Int instances...\n"
  checkEq prxInt
  checkOrd prxInt
  checkSemiring prxInt
  checkEuclideanRing prxInt
  checkRing prxInt
