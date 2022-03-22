module Test.Data.Tuple where

import Prelude

import Data.Tuple (Tuple)
import Effect (Effect)
import Test.QuickCheck.Laws (A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkTuple ∷ Effect Unit
checkTuple = checkLaws "Tuple" do
  Data.checkEq prxTuple
  Data.checkOrd prxTuple
  Data.checkBounded prxTuple
  Data.checkEnum prxTuple
  Data.checkSemigroup prxTuple
  Data.checkMonoid prxTuple
  Data.checkFunctor prx2Tuple
  Data.checkFunctorWithIndex prx2Tuple
  Data.checkFoldableFunctor prx2Tuple
  Control.checkSemigroupoid prx3Tuple
  Control.checkApply prx2Tuple
  Control.checkApplicative prx2Tuple
  Control.checkBind prx2Tuple
  Control.checkMonad prx2Tuple
  Control.checkExtend prx2Tuple
  Control.checkComonad prx2Tuple
  where
  prxTuple = Proxy ∷ Proxy (Tuple A B)
  prx2Tuple = Proxy ∷ Proxy (Tuple C)
  prx3Tuple = Proxy ∷ Proxy Tuple
