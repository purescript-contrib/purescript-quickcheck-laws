module Test.Data.Maybe (checkMaybe) where

import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe())
import Test.QuickCheck.Laws.Control.Alt
import Test.QuickCheck.Laws.Control.Alternative
import Test.QuickCheck.Laws.Control.Applicative
import Test.QuickCheck.Laws.Control.Apply
import Test.QuickCheck.Laws.Control.Bind
import Test.QuickCheck.Laws.Control.Extend
import Test.QuickCheck.Laws.Control.Monad
import Test.QuickCheck.Laws.Control.MonadPlus
import Test.QuickCheck.Laws.Control.Plus
import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Functor
import Test.QuickCheck.Laws.Data.Monoid
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
import Type.Proxy (Proxy(..), Proxy2(..))

import Prelude

prxMaybe2 :: Proxy2 Maybe
prxMaybe2 = Proxy2

prxA :: Proxy Boolean
prxA = Proxy

prxB :: Proxy String
prxB = Proxy

prxC :: Proxy Int
prxC = Proxy

checkMaybe = do
  log "\n\nChecking Maybe instances...\n"
  checkFunctor prxMaybe2 prxA prxB
  checkApply prxMaybe2 prxA prxB prxC
  checkApplicative prxMaybe2 prxA prxB prxC
  checkAlt prxMaybe2 prxA prxB
  checkPlus prxMaybe2 prxA prxB
  checkAlternative prxMaybe2 prxA prxB
  checkBind prxMaybe2 prxA
  checkMonad prxMaybe2 prxA
  -- TODO: Maybe is not a law-abiding MonadPlus
  -- checkMonadPlus prxMaybe2 prxA prxB
  checkExtend prxMaybe2 prxA prxB prxC
  checkSemigroup (Proxy :: Proxy (Maybe String))
  checkEq (Proxy :: Proxy (Maybe Int))
  checkOrd (Proxy :: Proxy (Maybe Int))
  checkBounded (Proxy :: Proxy (Maybe Boolean))
  checkMonoid (Proxy :: Proxy (Maybe String))
