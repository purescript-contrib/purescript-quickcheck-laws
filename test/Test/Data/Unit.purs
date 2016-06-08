module Test.Data.Unit (checkUnit) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Bounded (checkBounded)
import Test.QuickCheck.Laws.Data.CommutativeRing (checkCommutativeRing)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.EuclideanRing (checkEuclideanRing)
import Test.QuickCheck.Laws.Data.Field (checkField)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Ring (checkRing)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Test.QuickCheck.Laws.Data.Semiring (checkSemiring)
import Type.Proxy (Proxy(..))

import Prelude

prxUnit :: Proxy Unit
prxUnit = Proxy

checkUnit = do
  log "\n\nChecking Unit instances...\n"
  checkEq prxUnit
  checkOrd prxUnit
  checkBounded prxUnit
  checkSemigroup prxUnit
  checkMonoid prxUnit
  checkSemiring prxUnit
  checkEuclideanRing prxUnit
  checkRing prxUnit
  checkField prxUnit
  checkCommutativeRing prxUnit
