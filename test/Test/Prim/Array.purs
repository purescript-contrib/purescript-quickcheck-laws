module Test.Prim.Array where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..), Proxy2(..))

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
  Control.checkMonadZero prx2Array
  Control.checkMonadPlus prx2Array
  Data.checkEqShow prxArray
  Data.checkOrdShow prxArray
  Data.checkFunctorShow prx2Array
  Data.checkFoldableFunctorShow prx2Array
  Control.checkApplyShow prx2Array
  Control.checkApplicativeShow prx2Array
  Control.checkBindShow prx2Array
  Control.checkMonadShow prx2Array
  Data.checkSemigroupShow prxArray
  Data.checkMonoidShow prxArray
  Control.checkAltShow prx2Array
  Control.checkPlusShow prx2Array
  Control.checkAlternativeShow prx2Array
  Control.checkMonadZeroShow prx2Array
  Control.checkMonadPlusShow prx2Array
  where
  prxArray = Proxy ∷ Proxy (Array A)
  prx2Array = Proxy2 ∷ Proxy2 Array
