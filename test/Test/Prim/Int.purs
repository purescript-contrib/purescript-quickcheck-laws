module Test.Prim.Int where

import Prelude

import Effect (Effect)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

-- | Necessary for the EuclideanRing test
-- | so as to prevent integer overflow when multiplying large integer values
newtype EuclideanRingInt = EuclideanRingInt Int
derive newtype instance eqEuclideanRingInt :: Eq EuclideanRingInt
derive newtype instance semiringEuclideanRingInt :: Semiring EuclideanRingInt
derive newtype instance ringEuclideanRingInt :: Ring EuclideanRingInt
derive newtype instance commutativeRingEuclideanRingInt :: CommutativeRing EuclideanRingInt
derive newtype instance euclideanRingEuclideanRingInt :: EuclideanRing EuclideanRingInt
instance arbitraryEuclideanRingInt :: Arbitrary EuclideanRingInt where
  arbitrary = do
    EuclideanRingInt <$> chooseInt (-10_000) 10_000

checkInt ∷ Effect Unit
checkInt = checkLaws "Int" do
  Data.checkEq prxInt
  Data.checkOrd prxInt
  Data.checkCommutativeRing prxInt
  Data.checkSemiring prxInt
  Data.checkEuclideanRing prxEuclideanRingInt
  Data.checkRing prxInt
  where
  prxInt = Proxy ∷ Proxy Int
  prxEuclideanRingInt = Proxy ∷ Proxy EuclideanRingInt
