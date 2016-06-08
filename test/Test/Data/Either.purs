module Test.Data.Either (checkEither) where

import Control.Monad.Eff.Console (log)
import Data.Either (Either())
import Test.QuickCheck.Laws (A, B, C)
import Test.QuickCheck.Laws.Control.Alt (checkAlt)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Extend (checkExtend)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Data.Bounded (checkBounded)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
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
