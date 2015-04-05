module Test.QuickCheck.Laws.Control.Category where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Identity: `id <<< p = p <<< id = p`
checkCategory :: forall a b. (Category a, Arbitrary (a b b), Eq (a b b)) => a b b -> QC Unit
checkCategory _ = do

  trace "Checking 'Identity' law for Category"
  quickCheck identity

  where

  identity :: a b b -> Boolean
  identity p = (id <<< p) == p
            && (p <<< id) == p
