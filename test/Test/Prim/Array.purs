module Test.Prim.Array (checkArray) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws
import Test.QuickCheck.Laws.Control.Alt
import Test.QuickCheck.Laws.Control.Alternative
import Test.QuickCheck.Laws.Control.Applicative
import Test.QuickCheck.Laws.Control.Apply
import Test.QuickCheck.Laws.Control.Bind
import Test.QuickCheck.Laws.Control.Monad
import Test.QuickCheck.Laws.Control.MonadPlus
import Test.QuickCheck.Laws.Control.Plus
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Functor
import Test.QuickCheck.Laws.Data.Monoid
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
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
