module Test.QuickCheck.Laws.Data.Ring where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (Arbitrary)

-- | - Additive inverse: `a + (-a) = (-a) + a = zero`
checkRing :: forall eff a. (Ring a, Arbitrary a, Eq a) => Proxy a -> QC eff Unit
checkRing _ = do

  log "Checking 'Additive inverse' law for Ring"
  quickCheck' 1000 additiveInverse

  where

  additiveInverse :: a -> Boolean
  additiveInverse a = a + (-a) == zero
