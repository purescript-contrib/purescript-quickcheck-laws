module Test.Prim.Number where

import Test.QuickCheck
import Test.QuickCheck.Laws.Data.DivisionRing
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.ModuloSemiring
import Test.QuickCheck.Laws.Data.Num
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Ring
import Test.QuickCheck.Laws.Data.Semiring
import Type.Proxy (Proxy(..))

newtype NumberApprox = NumberApprox Number

prxNumber :: Proxy NumberApprox
prxNumber = Proxy

infix 4 =~=

-- Approximate equality to overcome precision issues
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

instance arbitraryNumberApprox :: Arbitrary NumberApprox where
  arbitrary = NumberApprox <$> arbitrary

instance eqNumberApprox :: Eq NumberApprox where
  (==) (NumberApprox x) (NumberApprox y) = x =~= y
  (/=) (NumberApprox x) (NumberApprox y) = not (x =~= y)

instance ordNumberApprox :: Ord NumberApprox where
  compare (NumberApprox x) (NumberApprox y) = compare x y

instance semiringNumberApprox :: Semiring NumberApprox where
  (+) (NumberApprox x) (NumberApprox y) = NumberApprox (x + y)
  zero = NumberApprox zero
  (*) (NumberApprox x) (NumberApprox y) = NumberApprox (x * y)
  one = NumberApprox one

instance moduloSemiringNumberApprox :: ModuloSemiring NumberApprox where
  (/) (NumberApprox x) (NumberApprox y) = NumberApprox (x / y)
  mod (NumberApprox x) (NumberApprox y) = NumberApprox (x `mod` y)

instance ringNumberApprox :: Ring NumberApprox where
  (-) (NumberApprox x) (NumberApprox y) = NumberApprox (x - y)

instance divisionRingNumberApprox :: DivisionRing NumberApprox
instance numNumberApprox :: Num NumberApprox

checkNumber = do
  checkEq prxNumber
  checkOrd prxNumber
  checkSemiring prxNumber
  checkModuloSemiring prxNumber
  checkRing prxNumber
  checkDivisionRing prxNumber
  checkNum prxNumber
