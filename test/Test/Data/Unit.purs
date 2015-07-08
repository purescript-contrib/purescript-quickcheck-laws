module Test.Data.Unit (checkUnit) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.DivisionRing
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.ModuloSemiring
import Test.QuickCheck.Laws.Data.Monoid
import Test.QuickCheck.Laws.Data.Num
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Ring
import Test.QuickCheck.Laws.Data.Semigroup
import Test.QuickCheck.Laws.Data.Semiring
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
  checkModuloSemiring prxUnit
  checkRing prxUnit
  checkDivisionRing prxUnit
  checkNum prxUnit
