module Test.Prim.String where

import Prelude

import Test.QuickCheck.Laws (QC, checkLaws)
import Test.QuickCheck.Laws.Data as Data

import Type.Proxy (Proxy(..))

checkString ∷ ∀ eff. QC eff Unit
checkString = checkLaws "String" do
  Data.checkEq prxString
  Data.checkOrd prxString
  Data.checkSemigroup prxString
  Data.checkMonoid prxString
  where
  prxString = Proxy ∷ Proxy String
