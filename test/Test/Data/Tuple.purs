module Test.Data.Tuple (checkTuple) where

import Console (log)
import Data.Int (Int())
import Data.Tuple (Tuple())
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

prxTuple2 :: Proxy2 (Tuple Unit)
prxTuple2 = Proxy2

prxTuple3 :: Proxy3 Tuple
prxTuple3 = Proxy3

prxA :: Proxy Int
prxA = Proxy

prxB :: Proxy String
prxB = Proxy

prxC :: Proxy Boolean
prxC = Proxy

prxD :: Proxy Int
prxD = Proxy

checkTuple = do
  log "\n\nChecking Tuple instances...\n"
  checkEq (Proxy :: Proxy (Tuple String Int))
  checkOrd (Proxy :: Proxy (Tuple Int String))
  checkBounded (Proxy :: Proxy (Tuple Boolean Boolean))
  checkSemigroupoid prxTuple3 prxA prxB prxC prxD
  checkSemigroup (Proxy :: Proxy (Tuple String String))
  checkMonoid (Proxy :: Proxy (Tuple String String))
  checkFunctor prxTuple2 prxA prxB
  checkApply prxTuple2 prxA prxB prxC
  checkApplicative prxTuple2 prxA prxB prxC
  checkBind prxTuple2 prxA
  checkMonad prxTuple2 prxA
  checkExtend prxTuple2 prxA prxB prxC
  checkComonad prxTuple2 prxA prxB
