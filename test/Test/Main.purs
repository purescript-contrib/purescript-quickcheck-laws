module Test.Main where

import Control.Monad.Eff.Console (log)
import Test.Data.Either
import Test.Data.Int
import Test.Data.Maybe
import Test.Data.Ordering
import Test.Data.Tuple
import Test.Data.Unit
import Test.Prim.Array
import Test.Prim.Boolean
import Test.Prim.Number
import Test.Prim.String

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
