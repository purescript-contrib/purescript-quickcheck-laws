module Test.Prim.Array (checkArray) where

import Control.Monad.Eff.Console (log)
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

prxArray2 :: Proxy2 Array
prxArray2 = Proxy2

prxA :: Proxy Int
prxA = Proxy

prxB :: Proxy String
prxB = Proxy

prxC :: Proxy Boolean
prxC = Proxy

checkArray = do
  log "\n\nChecking Array instances...\n"
  checkEq (Proxy :: Proxy (Array Int))
  checkOrd (Proxy :: Proxy (Array Int))
  checkFunctor prxArray2 prxA prxB
  checkApply prxArray2 prxA prxB prxC
  checkApplicative prxArray2 prxA prxB prxC
  checkBind prxArray2 prxA
  checkMonad prxArray2 prxA
  checkSemigroup (Proxy :: Proxy (Array String))
  checkMonoid (Proxy :: Proxy (Array String))
  checkAlt prxArray2 prxA prxB
  checkPlus prxArray2 prxA prxB
  checkAlternative prxArray2 prxA prxB
  checkMonadPlus prxArray2 prxA prxB
