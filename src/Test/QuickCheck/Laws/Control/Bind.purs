module Test.QuickCheck.Laws.Control.Bind where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
checkBind :: forall m. (Bind m, Arbitrary1 m, Eq1 m) => Proxy2 m -> QC Unit
checkBind _ = do

  log "Checking 'Associativity' law for Bind"
  quickCheck associativity

  where

  associativity :: Wrap m A -> (A -> Wrap m B) -> (B -> Wrap m C) -> Boolean
  associativity (Wrap m) f g = ((m >>= f') >>= g') `eq1` (m >>= (\x -> f' x >>= g'))
    where
    f' = f >>> unwrap
    g' = g >>> unwrap