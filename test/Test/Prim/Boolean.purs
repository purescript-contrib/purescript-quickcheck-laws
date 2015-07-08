module Test.Prim.Boolean (checkBoolean) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Bounded
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Ord
import Type.Proxy (Proxy(..))

import Prelude

prxBoolean :: Proxy Boolean
prxBoolean = Proxy

checkBoolean = do
  log "\n\nChecking Boolean instances...\n"
  checkEq prxBoolean
  checkOrd prxBoolean
  checkBounded prxBoolean
