module Test.Data.Maybe (checkMaybe) where

import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe())
import Test.QuickCheck.Laws
import Test.QuickCheck.Laws.Control.Alt
import Test.QuickCheck.Laws.Control.Alternative
import Test.QuickCheck.Laws.Control.Applicative
import Test.QuickCheck.Laws.Control.Apply
import Test.QuickCheck.Laws.Control.Bind
import Test.QuickCheck.Laws.Control.Extend
import Test.QuickCheck.Laws.Control.Monad
import Test.QuickCheck.Laws.Control.MonadZero
import Test.QuickCheck.Laws.Control.Plus
import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Functor
import Test.QuickCheck.Laws.Data.Monoid
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
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
