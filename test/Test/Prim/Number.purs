module Test.Prim.Number where

import Prelude

import Effect (Effect)
import Test.QuickCheck.Arbitrary (class Coarbitrary, class Arbitrary)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

checkNumber ∷ Effect Unit
checkNumber = checkLaws "Number" do
  Data.checkEq prxNumber
  Data.checkOrd prxNumber
  Data.checkSemiring prxNumber
  Data.checkRing prxNumber
  Data.checkEuclideanRing prxNumber
  Data.checkDivisionRing prxNumber
  Data.checkCommutativeRing prxNumber
  where
  prxNumber = Proxy ∷ Proxy ApproxNumber

newtype ApproxNumber = ApproxNumber Number

approximateEqual :: Number -> Number -> Boolean
approximateEqual x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

infix 2 approximateEqual as =~=

derive newtype instance arbitraryApproxNumber :: Arbitrary ApproxNumber
derive newtype instance coarbitraryApproxNumber :: Coarbitrary ApproxNumber

instance eqApproxNumber :: Eq ApproxNumber where
  eq (ApproxNumber x) (ApproxNumber y) = x =~= y

derive newtype instance ordApproxNumber :: Ord ApproxNumber
derive newtype instance semiringApproxNumber :: Semiring ApproxNumber
derive newtype instance ringApproxNumber :: Ring ApproxNumber
derive newtype instance commutativeRingApproxNumber :: CommutativeRing ApproxNumber
derive newtype instance euclideanRingApproxNumber :: EuclideanRing ApproxNumber
derive newtype instance divisionRingApproxNumber :: DivisionRing ApproxNumber
