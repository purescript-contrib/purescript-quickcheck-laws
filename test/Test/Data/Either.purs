module Test.Data.Either (checkEither) where

import Control.Monad.Eff.Console (log)
import Data.Either (Either())
import Test.QuickCheck.Laws
import Test.QuickCheck.Laws.Control.Alt
import Test.QuickCheck.Laws.Control.Applicative
import Test.QuickCheck.Laws.Control.Apply
import Test.QuickCheck.Laws.Control.Bind
import Test.QuickCheck.Laws.Control.Extend
import Test.QuickCheck.Laws.Control.Monad
import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Functor
import Test.QuickCheck.Laws.Data.Ord
import Type.Proxy (Proxy(..), Proxy2(..))

import Prelude

prxEither :: Proxy (Either A B)
prxEither = Proxy

prx2Either :: Proxy2 (Either C)
prx2Either = Proxy2

checkEither = do
  log "\n\nChecking Either instances...\n"
  checkFunctor prx2Either
  checkApply prx2Either
  checkApplicative prx2Either
  checkAlt prx2Either
  checkBind prx2Either
  checkMonad prx2Either
  checkExtend prx2Either
  checkEq prxEither
  checkOrd prxEither
  checkBounded prxEither
