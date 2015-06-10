module Test.QuickCheck.Laws.Control.MonadPlus where

import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.MonadPlus (MonadPlus)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
-- | - Annihilation: `empty >>= f = empty`
checkMonadPlus :: forall m. (MonadPlus m, Arbitrary1 m, Eq1 m) => Proxy2 m -> QC Unit
checkMonadPlus _ = do

  log "Checking 'Distributivity' law for MonadPlus"
  quickCheck distributivity

  log "Checking 'Annihilation' law for MonadPlus"
  quickCheck annihilation

  where

  distributivity :: Wrap m A -> Wrap m A -> (A -> Wrap m B) -> Boolean
  distributivity (Wrap x) (Wrap y) f = ((x <|> y) >>= f') `eq1` ((x >>= f') <|> (y >>= f'))
    where
    f' = f >>> unwrap

  annihilation :: (A -> Wrap m B) -> Boolean
  annihilation f = (empty >>= f') `eq1` empty
    where
    f' = f >>> unwrap
