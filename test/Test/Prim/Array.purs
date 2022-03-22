module Test.Prim.Array where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkArray ∷ Effect Unit
checkArray = checkLaws "Array" do
  Data.checkEq prxArray
  Data.checkOrd prxArray
  Data.checkFunctor prx2Array
  Data.checkFoldableFunctor prx2Array
  Control.checkApply prx2Array
  Control.checkApplicative prx2Array
  Control.checkBind prx2Array
  Control.checkMonad prx2Array
  Data.checkSemigroup prxArray
  Data.checkMonoid prxArray
  Control.checkAlt prx2Array
  Control.checkPlus prx2Array
  Control.checkAlternative prx2Array
  Control.checkMonadPlus prx2Array
  where
  prxArray = Proxy ∷ Proxy (Array A)
  prx2Array = Proxy ∷ Proxy Array
