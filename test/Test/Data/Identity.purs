module Test.Data.Identity where

import Prelude

import Data.Identity (Identity)
import Effect (Effect)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

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
  Data.checkFunctorWithIndex prx2Identity
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
  where
  prxIdentity = Proxy ∷ Proxy (Identity A)
  prx2Identity = Proxy ∷ Proxy Identity
