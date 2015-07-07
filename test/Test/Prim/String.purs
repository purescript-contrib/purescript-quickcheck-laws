module Test.Prim.String (checkString) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Monoid
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semigroup
import Type.Proxy (Proxy(..))

import Prelude

prxString :: Proxy String
prxString = Proxy

checkString = do
  log "\n\nChecking String instances...\n"
  checkEq prxString
  checkOrd prxString
  checkSemigroup prxString
  checkMonoid prxString
