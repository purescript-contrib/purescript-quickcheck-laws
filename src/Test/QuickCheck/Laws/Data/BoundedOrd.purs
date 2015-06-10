module Test.QuickCheck.Laws.Data.BoundedOrd where

import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)

import Type.Proxy (Proxy())

-- | - Ordering: `bottom <= a <= top`
checkBoundedOrd :: forall a. (Arbitrary a, BoundedOrd a) => Proxy a -> QC Unit
checkBoundedOrd _ = do

  log "Checking 'Ordering' law for BoundedOrd"
  quickCheck ordering

  where

  ordering :: a -> Boolean
  ordering a = bottom <= a && a <= top
