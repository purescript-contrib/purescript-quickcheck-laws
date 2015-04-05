module Test.Prim.Array where

import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Ord
import Type.Proxy

prxArray :: Proxy [Number]
prxArray = Proxy

checkArray = do
  checkEq prxArray
  checkOrd prxArray
