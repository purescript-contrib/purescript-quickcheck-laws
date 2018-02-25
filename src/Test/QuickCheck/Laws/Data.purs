module Test.QuickCheck.Laws.Data (module Exports) where

import Test.QuickCheck.Laws.Data.BooleanAlgebra (checkBooleanAlgebra, checkBooleanAlgebraGen) as Exports
import Test.QuickCheck.Laws.Data.Bounded (checkBounded, checkBoundedGen) as Exports
import Test.QuickCheck.Laws.Data.CommutativeRing (checkCommutativeRing, checkCommutativeRingGen) as Exports
import Test.QuickCheck.Laws.Data.Eq (checkEq, checkEqGen) as Exports
import Test.QuickCheck.Laws.Data.BoundedEnum (checkBoundedEnum, checkBoundedEnumGen) as Exports
import Test.QuickCheck.Laws.Data.EuclideanRing (checkEuclideanRing, checkEuclideanRingGen) as Exports
import Test.QuickCheck.Laws.Data.Field (checkField, checkFieldGen) as Exports
import Test.QuickCheck.Laws.Data.Foldable (checkFoldable,  checkFoldableGen, checkFoldableFunctor, checkFoldableFunctorGen) as Exports
import Test.QuickCheck.Laws.Data.Functor (checkFunctor, checkFunctorGen) as Exports
import Test.QuickCheck.Laws.Data.HeytingAlgebra (checkHeytingAlgebra, checkHeytingAlgebraGen) as Exports
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid, checkMonoidGen) as Exports
import Test.QuickCheck.Laws.Data.Ord (checkOrd, checkOrdGen) as Exports
import Test.QuickCheck.Laws.Data.Ring (checkRing, checkRingGen) as Exports
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup, checkSemigroupGen) as Exports
import Test.QuickCheck.Laws.Data.Semiring (checkSemiring, checkSemiringGen) as Exports
