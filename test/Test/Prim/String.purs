module Test.Prim.String (checkString) where

import Console (log)
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Monoid
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
import Type.Proxy (Proxy(..))

prxString :: Proxy String
prxString = Proxy

checkString = do
  log "\n\nChecking String instances...\n"
  checkEq prxString
  checkOrd prxString
  checkSemigroup prxString
  checkMonoid prxString
