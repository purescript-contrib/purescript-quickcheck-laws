module Test.Data.Tuple (checkTuple) where

import Control.Monad.Eff.Console (log)
import Data.Tuple (Tuple())
import Test.QuickCheck.Laws (A, B, C)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Comonad (checkComonad)
import Test.QuickCheck.Laws.Control.Extend (checkExtend)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.Semigroupoid (checkSemigroupoid)
import Test.QuickCheck.Laws.Data.Bounded (checkBounded)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Type.Proxy (Proxy(..), Proxy2(..), Proxy3(..))

import Prelude

prxTuple :: Proxy (Tuple A B)
prxTuple = Proxy

prx2Tuple :: Proxy2 (Tuple C)
prx2Tuple = Proxy2

prx3Tuple :: Proxy3 Tuple
prx3Tuple = Proxy3

checkTuple = do
  log "\n\nChecking Tuple instances...\n"
  checkEq prxTuple
  checkOrd prxTuple
  checkBounded prxTuple
  checkSemigroupoid prx3Tuple
  checkSemigroup prxTuple
  checkMonoid prxTuple
  checkFunctor prx2Tuple
  checkApply prx2Tuple
  checkApplicative prx2Tuple
  checkBind prx2Tuple
  checkMonad prx2Tuple
  checkExtend prx2Tuple
  checkComonad prx2Tuple
