module Test.QuickCheck.Laws.Data.Ring where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

import Prelude

-- | - Additive inverse: `a + (-a) = (-a) + a = zero`
checkRing :: forall a eff. (Ring a, Arbitrary a, Eq a) => Proxy a -> QC eff Unit
checkRing _ = do

  log "Checking 'Additive inverse' law for Ring"
  quickCheck additiveInverse

  where

  additiveInverse :: a -> Boolean
  additiveInverse a = a + (-a) == zero
