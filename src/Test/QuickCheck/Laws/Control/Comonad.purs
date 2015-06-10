module Test.QuickCheck.Laws.Control.Comonad where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Control.Comonad (Comonad, extract)
import Control.Extend (extend)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Left Identity: `extract <<= x = x`
-- | - Right Identity: `extract (f <<= x) = f x`
checkComonad :: forall w. (Comonad w, Coarbitrary1 w, Arbitrary1 w, Eq1 w) => Proxy2 w -> QC Unit
checkComonad _ = do

  log "Checking 'Left identity' law for Comonad"
  quickCheck leftIdentity

  log "Checking 'Right identity' law for Comonad"
  quickCheck rightIdentity

  where

  leftIdentity :: Wrap w A -> Boolean
  leftIdentity (Wrap x) = extend extract x `eq1` x

  rightIdentity :: (Wrap w A -> B) -> Wrap w A -> Boolean
  rightIdentity f (Wrap x) = extract (extend f' x) == f' x
    where
    f' = f <<< Wrap
