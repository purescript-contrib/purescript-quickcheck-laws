module Test.QuickCheck.Laws.Data.EuclideanRing where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | - Remainder: ```a / b * b + (a `mod` b) = a```
checkEuclideanRing
  :: forall eff a. (EuclideanRing a, Arbitrary a, Eq a)
  => Proxy a -> QC eff Unit
checkEuclideanRing _ = do

  log "Checking 'Remainder' law for EuclideanRing"
  quickCheck' 1000 remainder

  where

  remainder :: a -> a -> Boolean
  remainder a b = a / b * b + (a `mod` b) == a
