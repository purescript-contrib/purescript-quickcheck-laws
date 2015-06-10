module Test.QuickCheck.Laws.Control.Category where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)

import Type.Proxy (Proxy(), Proxy3())

-- | - Identity: `id <<< p = p <<< id = p`
checkCategory :: forall a b c. (Category a,
                                Arbitrary (a b c),
                                Eq (a b c)) => Proxy3 a
                                            -> Proxy b
                                            -> Proxy c
                                            -> QC Unit
checkCategory _ _ _ = do

  log "Checking 'Identity' law for Category"
  quickCheck identity

  where

  identity :: a b c -> Boolean
  identity p = (id <<< p) == p
            && (p <<< id) == p
