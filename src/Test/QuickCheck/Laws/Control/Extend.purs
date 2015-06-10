module Test.QuickCheck.Laws.Control.Extend where

import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Control.Extend (Extend, extend)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Associativity: `extend f <<< extend g = extend (f <<< extend g)`
checkExtend :: forall w. (Extend w, Arbitrary1 w, Coarbitrary1 w, Eq1 w) => Proxy2 w -> QC Unit
checkExtend _ = do

  log "Checking 'Associativity' law for Extend"
  quickCheck associativity

  where

  associativity :: (Wrap w B -> C) -> (Wrap w A -> B) -> Wrap w A -> Boolean
  associativity f g (Wrap x) = (extend f' <<< extend g') x `eq1` extend (f' <<< (extend g')) x
    where
    f' = f <<< Wrap
    g' = g <<< Wrap
