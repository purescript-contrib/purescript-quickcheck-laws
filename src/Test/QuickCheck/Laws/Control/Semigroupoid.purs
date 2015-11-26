module Test.QuickCheck.Laws.Control.Semigroupoid where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy3())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Laws (B(), C(), D(), E())

-- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
checkSemigroupoid :: forall a. (Semigroupoid a, Arbitrary (a B C), Arbitrary (a C D), Arbitrary (a D E), Eq (a B E)) => Proxy3 a -> QC () Unit
checkSemigroupoid _ = do

  log "Checking 'Associativity' law for Semigroupoid"
  quickCheck' 1000 associativity

  where

  associativity :: a D E -> a C D -> a B C -> Boolean
  associativity p q r = (p <<< (q <<< r)) == ((p <<< q) <<< r)
