module Test.Prim.String (checkString) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
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
