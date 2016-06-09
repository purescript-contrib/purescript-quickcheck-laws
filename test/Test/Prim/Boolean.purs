module Test.Prim.Boolean (checkBoolean) where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck.Laws.Data.Bounded (checkBounded)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Type.Proxy (Proxy(..))

import Prelude

prxBoolean :: Proxy Boolean
prxBoolean = Proxy

checkBoolean = do
  log "\n\nChecking Boolean instances...\n"
  checkEq prxBoolean
  checkOrd prxBoolean
  checkBounded prxBoolean
