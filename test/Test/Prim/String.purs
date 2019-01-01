module Test.Prim.String where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkString ∷ Effect Unit
checkString = checkLaws "String" do
  Data.checkEq prxString
  Data.checkOrd prxString
  Data.checkSemigroup prxString
  Data.checkMonoid prxString
  Data.checkEqShow prxString
  Data.checkOrdShow prxString
  Data.checkSemigroupShow prxString
  Data.checkMonoidShow prxString
  where
  prxString = Proxy ∷ Proxy String
