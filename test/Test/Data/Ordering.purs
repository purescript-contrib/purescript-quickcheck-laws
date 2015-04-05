module Test.Data.Ordering where

import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
import Type.Proxy (Proxy(..))

prxOrdering :: Proxy Ordering
prxOrdering = Proxy

checkOrdering = do
  checkEq prxOrdering
  checkOrd prxOrdering
  checkBounded prxOrdering
  checkSemigroup prxOrdering
