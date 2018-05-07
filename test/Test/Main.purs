module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Either (checkEither)
import Test.Data.Identity (checkIdentity)
import Test.Data.List (checkList)
import Test.Data.Maybe (checkMaybe)
import Test.Data.Ordering (checkOrdering)
import Test.Data.Tuple (checkTuple)
import Test.Data.Unit (checkUnit)
import Test.Prim.Array (checkArray)
import Test.Prim.Boolean (checkBoolean)
import Test.Prim.Int (checkInt)
import Test.Prim.Number (checkNumber)
import Test.Prim.String (checkString)

main ∷ Effect Unit
main = do
  checkArray
  checkBoolean
  checkInt
  checkNumber
  checkString

  checkEither
  checkIdentity
  checkList
  checkMaybe
  checkOrdering
  checkTuple
  checkUnit
