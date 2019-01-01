module Test.Data.Maybe where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..), Proxy2(..))

checkMaybe ∷ Effect Unit
checkMaybe = checkLaws "Maybe" do
  Data.checkEq prxMaybe
  Data.checkOrd prxMaybe
  Data.checkBounded prxMaybe
  Data.checkSemigroup prxMaybe
  Data.checkMonoid prxMaybe
  Data.checkFunctor prx2Maybe
  Data.checkFoldableFunctor prx2Maybe
  Control.checkApply prx2Maybe
  Control.checkApplicative prx2Maybe
  Control.checkAlt prx2Maybe
  Control.checkPlus prx2Maybe
  Control.checkAlternative prx2Maybe
  Control.checkBind prx2Maybe
  Control.checkMonad prx2Maybe
  Control.checkMonadZero prx2Maybe
  Control.checkExtend prx2Maybe
  Data.checkEqShow prxMaybe
  Data.checkOrdShow prxMaybe
  Data.checkBoundedShow prxMaybe
  Data.checkSemigroupShow prxMaybe
  Data.checkMonoidShow prxMaybe
  Data.checkFunctorShow prx2Maybe
  Data.checkFoldableFunctorShow prx2Maybe
  Control.checkApplyShow prx2Maybe
  Control.checkApplicativeShow prx2Maybe
  Control.checkAltShow prx2Maybe
  Control.checkPlusShow prx2Maybe
  Control.checkAlternativeShow prx2Maybe
  Control.checkBindShow prx2Maybe
  Control.checkMonadShow prx2Maybe
  Control.checkMonadZeroShow prx2Maybe
  Control.checkExtendShow prx2Maybe
  where
  prxMaybe = Proxy ∷ Proxy (Maybe A)
  prx2Maybe = Proxy2 ∷ Proxy2 Maybe
