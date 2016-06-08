module Test.Data.Maybe (checkMaybe) where

import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe())
import Test.QuickCheck.Laws (A)
import Test.QuickCheck.Laws.Control.Alt (checkAlt)
import Test.QuickCheck.Laws.Control.Alternative (checkAlternative)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Extend (checkExtend)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.MonadZero (checkMonadZero)
import Test.QuickCheck.Laws.Control.Plus (checkPlus)
import Test.QuickCheck.Laws.Data.Bounded (checkBounded)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Type.Proxy (Proxy(..), Proxy2(..))

import Prelude

prxMaybe :: Proxy (Maybe A)
prxMaybe = Proxy

prx2Maybe :: Proxy2 Maybe
prx2Maybe = Proxy2

checkMaybe = do
  log "\n\nChecking Maybe instances...\n"
  checkFunctor prx2Maybe
  checkApply prx2Maybe
  checkApplicative prx2Maybe
  checkAlt prx2Maybe
  checkPlus prx2Maybe
  checkAlternative prx2Maybe
  checkBind prx2Maybe
  checkMonad prx2Maybe
  checkMonadZero prx2Maybe
  checkExtend prx2Maybe
  checkSemigroup prxMaybe
  checkEq prxMaybe
  checkOrd prxMaybe
  checkBounded prxMaybe
  checkMonoid prxMaybe
