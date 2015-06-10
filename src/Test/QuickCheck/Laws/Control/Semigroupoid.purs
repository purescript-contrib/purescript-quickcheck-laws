module Test.QuickCheck.Laws.Control.Semigroupoid where

import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy3())

-- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
checkSemigroupoid :: forall a b c d e. (Semigroupoid a, Arbitrary (a A A), Eq (a A A)) => Proxy3 a -> QC Unit
checkSemigroupoid _ = do

  log "Checking 'Associativity' law for Semigroupoid"
  quickCheck associativity

  where

  associativity :: a A A -> a A A -> a A A -> Boolean
  associativity p q r = (p <<< (q <<< r)) == ((p <<< q) <<< r)
