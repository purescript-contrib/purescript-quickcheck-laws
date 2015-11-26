module Test.QuickCheck.Laws.Control.MonadPlus where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff.Console (log)
import Control.MonadPlus (MonadPlus)
import Control.Plus (empty)

import Type.Proxy (Proxy2())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Laws (A(), B())

-- | - Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
-- | - Annihilation: `empty >>= f = empty`
checkMonadPlus :: forall m. (MonadPlus m, Arbitrary (m A), Arbitrary (m B), Eq (m B)) => Proxy2 m -> QC () Unit
checkMonadPlus _ = do

  log "Checking 'Distributivity' law for MonadPlus"
  quickCheck' 1000 distributivity

  log "Checking 'Annihilation' law for MonadPlus"
  quickCheck' 1000 annihilation

  where

  distributivity :: m A -> m A -> (A -> m B) -> Boolean
  distributivity x y f = ((x <|> y) >>= f) == ((x >>= f) <|> (y >>= f))

  annihilation :: (A -> m B) -> Boolean
  annihilation f = (empty >>= f) == empty
