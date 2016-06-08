module Test.QuickCheck.Laws.Data.CommutativeRing where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Commutative multiplication: `a * b = b * a`
checkCommutativeRing
  :: forall eff a. (CommutativeRing a, Arbitrary a, Eq a) => Proxy a -> QC eff Unit
checkCommutativeRing _ = do

  log "Checking 'Commutative multiplication' law for CommutativeRing"
  quickCheck' 1000 commutativeMultiplication

  where

  commutativeMultiplication :: a -> a -> Boolean
  commutativeMultiplication a b = a * b == b * a
