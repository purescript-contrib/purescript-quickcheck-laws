module Test.QuickCheck.Laws.Control.Plus where

import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Control.Alt ((<|>))
import Control.Plus (Plus, empty)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Left identity: `empty <|> x == x`
-- | - Right identity: `x <|> empty == x`
-- | - Annihilation: `f <$> empty == empty`
checkPlus :: forall f. (Plus f, Arbitrary1 f, Eq1 f) => Proxy2 f -> QC Unit
checkPlus _ = do

  log "Checking 'Left identity' law for Plus"
  quickCheck leftIdentity

  log "Checking 'Right identity' law for Plus"
  quickCheck rightIdentity

  log "Checking 'Annihilation' law for Plus"
  quickCheck annihilation

  where

  leftIdentity :: Wrap f A -> Boolean
  leftIdentity (Wrap x) = (empty <|> x) `eq1` x

  rightIdentity :: Wrap f A -> Boolean
  rightIdentity (Wrap x) = (x <|> empty) `eq1` x

  annihilation :: (A -> B) -> Boolean
  annihilation f = map f empty `eq1` (empty :: f B)
