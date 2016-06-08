module Test.QuickCheck.Laws.Control.Category where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy3())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (B(), C())

-- | - Identity: `id <<< p = p <<< id = p`
checkCategory :: forall eff a. (Category a, Arbitrary (a B C), Eq (a B C)) => Proxy3 a -> QC eff Unit
checkCategory _ = do

  log "Checking 'Identity' law for Category"
  quickCheck' 1000 identity

  where

  identity :: a B C -> Boolean
  identity p = (id <<< p) == p
            && (p <<< id) == p
