module Test.QuickCheck.Laws.Data.Ord where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
checkOrd :: forall a. (Arbitrary a, Ord a) => Proxy a -> QC Unit
checkOrd _ = do

  log "Checking 'Reflexivity' law for Ord"
  quickCheck reflexivity

  log "Checking 'Antisymmetry' law for Ord"
  quickCheck antisymmetry

  log "Checking 'Transitivity' law for Ord"
  quickCheck transitivity

  where

  reflexivity :: a -> Boolean
  reflexivity a = a <= a

  antisymmetry :: a -> a -> Boolean
  antisymmetry a b = if (a <= b) && (b <= a) then a == b else a /= b

  transitivity :: a -> a -> a -> Boolean
  transitivity a b c = if (a <= b) && (b <= c) then a <= c else true
