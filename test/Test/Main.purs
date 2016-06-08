module Test.Main where

import Test.Data.Either (checkEither)
import Test.Data.Int (checkInt)
import Test.Data.Maybe (checkMaybe)
import Test.Data.Ordering (checkOrdering)
import Test.Data.Tuple (checkTuple)
import Test.Data.Unit (checkUnit)
import Test.Prim.Array (checkArray)
import Test.Prim.Boolean (checkBoolean)
import Test.Prim.Number (checkNumber)
import Test.Prim.String (checkString)

import Prelude

main = do
  checkEither
  checkInt
  checkMaybe
  checkOrdering
  checkTuple
  checkUnit
  checkArray
  checkBoolean
  checkNumber
  checkString
