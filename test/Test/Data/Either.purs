module Test.Data.Either where

import Prelude

import Data.Either (Either)

import Test.QuickCheck.Laws (QC, A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data

import Type.Proxy (Proxy(..), Proxy2(..))

checkEither ∷ ∀ eff. QC eff Unit
checkEither = checkLaws "Either" do
  Data.checkEq prxEither
  Data.checkOrd prxEither
  Data.checkBounded prxEither
  Data.checkEnum prxEither  
  Data.checkBoundedEnum prxEither
  Data.checkFunctor prx2Either
  Data.checkFoldableFunctor prx2Either  
  Control.checkApply prx2Either
  Control.checkApplicative prx2Either
  Control.checkAlt prx2Either
  Control.checkBind prx2Either
  Control.checkMonad prx2Either
  Control.checkExtend prx2Either
  where
  prxEither = Proxy ∷ Proxy (Either A B)
  prx2Either = Proxy2 ∷ Proxy2 (Either C)
