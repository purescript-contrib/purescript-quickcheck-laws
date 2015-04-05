module Test.Main where

import Test.Data.Ordering
import Test.Data.Unit
import Test.Prim.Array
import Test.Prim.Boolean
import Test.Prim.Number
import Test.Prim.String
import Debug.Trace (trace)

main = do

  trace "Checking Boolean instances...\n"
  checkBoolean

  trace "\n\nChecking Number instances...\n"
  checkNumber

  trace "\n\nChecking String instances...\n"
  checkString

  trace "\n\nChecking Unit instances...\n"
  checkUnit

  trace "\n\nChecking Array instances...\n"
  checkArray

  trace "\n\nChecking Ordering instances...\n"
  checkOrdering
