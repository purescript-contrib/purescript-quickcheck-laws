module Test.Data.Ordering (checkOrdering) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
import Type.Proxy (Proxy(..))

import Prelude

prxOrdering :: Proxy Ordering
prxOrdering = Proxy

checkOrdering = do
  log "\n\nChecking Ordering instances...\n"
  checkEq prxOrdering
  checkOrd prxOrdering
  checkBounded prxOrdering
  checkSemigroup prxOrdering
