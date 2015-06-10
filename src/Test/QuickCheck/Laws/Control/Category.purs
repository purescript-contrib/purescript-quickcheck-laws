module Test.QuickCheck.Laws.Control.Category where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy3())

-- | - Identity: `id <<< p = p <<< id = p`
checkCategory :: forall a. (Category a, Arbitrary (a A A), Eq (a A A)) => Proxy3 a -> QC Unit
checkCategory _ = do

  log "Checking 'Identity' law for Category"
  quickCheck identity

  where

  identity :: a A A -> Boolean
  identity p = (id <<< p) == p
            && (p <<< id) == p
