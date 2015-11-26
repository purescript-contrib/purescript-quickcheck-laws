module Test.Data.Tuple (checkTuple) where

import Control.Monad.Eff.Console (log)
import Data.Tuple (Tuple())
import Test.QuickCheck.Laws
import Test.QuickCheck.Laws.Control.Alt
import Test.QuickCheck.Laws.Control.Applicative
import Test.QuickCheck.Laws.Control.Apply
import Test.QuickCheck.Laws.Control.Bind
import Test.QuickCheck.Laws.Control.Comonad
import Test.QuickCheck.Laws.Control.Extend
import Test.QuickCheck.Laws.Control.Monad
import Test.QuickCheck.Laws.Control.Semigroupoid
import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Functor
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
import Test.QuickCheck.Laws.Data.Monoid
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
