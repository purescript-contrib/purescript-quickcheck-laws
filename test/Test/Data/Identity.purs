module Test.Data.Identity where

import Prelude

import Data.Identity (Identity)
import Effect (Effect)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..), Proxy2(..))

checkIdentity ∷ Effect Unit
checkIdentity = checkLaws "Identity" do
  Data.checkEq prxIdentity
  Data.checkOrd prxIdentity
  Data.checkBounded prxIdentity

  Data.checkSemigroup prxIdentity
  Data.checkMonoid prxIdentity

  -- Data.checkSemiring prxIdentity
  -- Data.checkEuclideanRing prxIdentity
  -- Data.checkRing prxIdentity
  -- Data.checkCommutativeRing prxIdentity
  -- Data.checkField prxIdentity

  Data.checkFunctor prx2Identity
  Data.checkFoldableFunctor prx2Identity
  Control.checkApply prx2Identity
  Control.checkApplicative prx2Identity
  Control.checkBind prx2Identity
  Control.checkMonad prx2Identity

  Control.checkExtend prx2Identity
  Control.checkComonad prx2Identity

  -- checkAlt prx2Identity
  -- checkPlus prx2Identity
  -- checkAlternative prx2Identity
  -- checkMonadZero prx2Identity
  Data.checkEqShow prxIdentity
  Data.checkOrdShow prxIdentity
  Data.checkBoundedShow prxIdentity

  Data.checkSemigroupShow prxIdentity
  Data.checkMonoidShow prxIdentity

  -- Data.checkSemiringShow prxIdentity
  -- Data.checkEuclideanRingShow prxIdentity
  -- Data.checkRingShow prxIdentity
  -- Data.checkCommutativeRingShow prxIdentity
  -- Data.checkFieldShow prxIdentity

  Data.checkFunctorShow prx2Identity
  Data.checkFoldableFunctorShow prx2Identity
  Control.checkApplyShow prx2Identity
  Control.checkApplicativeShow prx2Identity
  Control.checkBindShow prx2Identity
  Control.checkMonadShow prx2Identity

  Control.checkExtendShow prx2Identity
  Control.checkComonadShow prx2Identity

  -- checkAltShow prx2Identity
  -- checkPlusShow prx2Identity
  -- checkAlternativeShow prx2Identity
  -- checkMonadZeroShow prx2Identity
  where
  prxIdentity = Proxy ∷ Proxy (Identity A)
  prx2Identity = Proxy2 ∷ Proxy2 Identity
