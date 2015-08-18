module Test.QuickCheck.Laws.Data.ModuloSemiring where

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy())

import Prelude

-- | - Remainder: ```a / b * b + (a `mod` b) = a```
checkModuloSemiring :: forall a eff. (ModuloSemiring a, Arbitrary a, Eq a) => Proxy a -> QC eff Unit
checkModuloSemiring _ = do

  log "Checking 'Remainder' law for ModuloSemiring"
  quickCheck remainder

  where

  remainder :: a -> a -> Boolean
  remainder a b = a / b * b + (a `mod` b) == a
