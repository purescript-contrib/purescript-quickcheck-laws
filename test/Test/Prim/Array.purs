module Test.Prim.Array (checkArray) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws (A)
import Test.QuickCheck.Laws.Control.Alt (checkAlt)
import Test.QuickCheck.Laws.Control.Alternative (checkAlternative)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.MonadPlus (checkMonadPlus)
import Test.QuickCheck.Laws.Control.Plus (checkPlus)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Type.Proxy (Proxy(..), Proxy2(..))

import Prelude

prxArray :: Proxy (Array A)
prxArray = Proxy

prx2Array :: Proxy2 Array
prx2Array = Proxy2

checkArray = do
  log "\n\nChecking Array instances...\n"
  checkEq prxArray
  checkOrd prxArray
  checkFunctor prx2Array
  checkApply prx2Array
  checkApplicative prx2Array
  checkBind prx2Array
  checkMonad prx2Array
  checkSemigroup prxArray
  checkMonoid prxArray
  checkAlt prx2Array
  checkPlus prx2Array
  checkAlternative prx2Array
  checkMonadPlus prx2Array
