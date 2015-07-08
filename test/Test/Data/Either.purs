module Test.Data.Either (checkEither) where

import Control.Monad.Eff.Console (log)
import Data.Either (Either())
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

prxEither2 :: Proxy2 (Either Unit)
prxEither2 = Proxy2

prxA :: Proxy Int
prxA = Proxy

prxB :: Proxy String
prxB = Proxy

prxC :: Proxy Boolean
prxC = Proxy

checkEither = do
  log "\n\nChecking Either instances...\n"
  checkFunctor prxEither2 prxA prxB
  checkApply prxEither2 prxA prxB prxC
  checkApplicative prxEither2 prxA prxB prxC
  checkAlt prxEither2 prxA prxB
  checkBind prxEither2 prxA
  checkMonad prxEither2 prxA
  checkExtend prxEither2 prxA prxB prxC
  checkEq (Proxy :: Proxy (Either String Int))
  checkOrd (Proxy :: Proxy (Either Int String))
  checkBounded (Proxy :: Proxy (Either Boolean Boolean))
