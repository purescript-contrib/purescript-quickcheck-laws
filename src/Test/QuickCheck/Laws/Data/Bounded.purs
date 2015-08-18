module Test.QuickCheck.Laws.Data.Bounded where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

import Prelude

-- | - Ordering: `bottom <= a <= top`
checkBounded :: forall a eff. (Arbitrary a, Bounded a, Ord a) => Proxy a -> QC eff Unit
checkBounded _ = do

  log "Checking 'Ordering' law for Bounded"
  quickCheck ordering

  where

  ordering :: a -> Boolean
  ordering a = bottom <= a && a <= top
