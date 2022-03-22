module Test.Data.Either where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Test.QuickCheck.Laws (A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkEither ∷ Effect Unit
checkEither = checkLaws "Either" do
  Data.checkEq prxEither
  Data.checkOrd prxEither
  Data.checkBounded prxEither
  Data.checkEnum prxEither
  Data.checkFunctor prx2Either
  Data.checkFunctorWithIndex prx2Either
  Data.checkFoldableFunctor prx2Either
  Control.checkApply prx2Either
  Control.checkApplicative prx2Either
  Control.checkAlt prx2Either
  Control.checkBind prx2Either
  Control.checkMonad prx2Either
  Control.checkExtend prx2Either
  where
  prxEither = Proxy ∷ Proxy (Either A B)
  prx2Either = Proxy ∷ Proxy (Either C)
