module Test.QuickCheck.Laws.Data.Num where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

import Prelude

-- | - Commutative multiplication: `a * b = b * a`
checkNum :: forall a. (Num a, Arbitrary a, Eq a) => Proxy a -> QC Unit
checkNum _ = do

  log "Checking 'Commutative multiplication' law for Num"
  quickCheck commutativeMultiplication

  where

  commutativeMultiplication :: a -> a -> Boolean
  commutativeMultiplication a b = a * b == b * a
