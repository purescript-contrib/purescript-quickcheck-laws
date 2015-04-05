module Test.Prim.String where

import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
import Type.Proxy (Proxy(..))

prxString :: Proxy String
prxString = Proxy

checkString = do
  checkEq prxString
  checkOrd prxString
  checkSemigroup prxString
