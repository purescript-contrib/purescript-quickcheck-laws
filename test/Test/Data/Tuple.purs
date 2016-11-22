module Test.Data.Tuple where

import Prelude

import Data.Tuple (Tuple)

import Test.QuickCheck.Laws (QC, A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data

import Type.Proxy (Proxy(..), Proxy2(..), Proxy3(..))

checkTuple ∷ ∀ eff. QC eff Unit
checkTuple = checkLaws "Tuple" do
  Data.checkEq prxTuple
  Data.checkOrd prxTuple
  Data.checkBounded prxTuple
  Data.checkEnum prxTuple  
  Data.checkBoundedEnum prxTuple
  Data.checkSemigroup prxTuple
  Data.checkMonoid prxTuple
  Data.checkFunctor prx2Tuple
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
  prx2Tuple = Proxy2 ∷ Proxy2 (Tuple C)
  prx3Tuple = Proxy3 ∷ Proxy3 Tuple
