module Test.Data.List where

import Prelude

import Data.List (List)
import Effect (Effect)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkList ∷ Effect Unit
checkList = checkLaws "List" do
  Data.checkEq prxList
  Data.checkOrd prxList
  Data.checkFunctor prx2List
  Data.checkFunctorWithIndex prx2List
  Data.checkFoldableFunctor prx2List
  Control.checkApply prx2List
  Control.checkApplicative prx2List
  Control.checkBind prx2List
  Control.checkMonad prx2List
  Data.checkSemigroup prxList
  Data.checkMonoid prxList
  Control.checkAlt prx2List
  Control.checkPlus prx2List
  Control.checkAlternative prx2List
  Control.checkMonadPlus prx2List
  where
  prxList = Proxy ∷ Proxy (List A)
  prx2List = Proxy ∷ Proxy List
