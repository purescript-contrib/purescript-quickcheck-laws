module Test.QuickCheck.Laws.Data.Bounded where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

-- | - Ordering: `bottom <= a <= top`
checkBounded :: forall a. (Arbitrary a, Bounded a) => Proxy a -> QC Unit
checkBounded _ = do

  log "Checking 'Ordering' law for Bounded"
  quickCheck ordering

  where

  ordering :: a -> Boolean
  ordering a = bottom <= a && a <= top
