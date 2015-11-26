module Test.QuickCheck.Laws.Data.ModuloSemiring where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (Arbitrary)

-- | - Remainder: ```a / b * b + (a `mod` b) = a```
checkModuloSemiring :: forall a. (ModuloSemiring a, Arbitrary a, Eq a) => Proxy a -> QC () Unit
checkModuloSemiring _ = do

  log "Checking 'Remainder' law for ModuloSemiring"
  quickCheck' 1000 remainder

  where

  remainder :: a -> a -> Boolean
  remainder a b = a / b * b + (a `mod` b) == a
