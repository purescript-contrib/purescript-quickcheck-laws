module Test.QuickCheck.Laws.Data.Bounded where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (Arbitrary)

-- | - Ordering: `bottom <= a <= top`
checkBounded :: forall a. (Arbitrary a, Bounded a, Ord a) => Proxy a -> QC () Unit
checkBounded _ = do

  log "Checking 'Ordering' law for Bounded"
  quickCheck' 1000 ordering

  where

  ordering :: a -> Boolean
  ordering a = bottom <= a && a <= top
