module Test.Prim.Boolean where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkBoolean ∷ Effect Unit
checkBoolean = checkLaws "Boolean" do
  Data.checkEq prxBoolean
  Data.checkOrd prxBoolean
  Data.checkBounded prxBoolean
  Data.checkEnum prxBoolean  
  Data.checkBoundedEnum prxBoolean
  Data.checkHeytingAlgebra prxBoolean
  Data.checkBooleanAlgebra prxBoolean
  where
  prxBoolean = Proxy ∷ Proxy Boolean
