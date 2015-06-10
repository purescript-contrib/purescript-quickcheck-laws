module Test.QuickCheck.Laws.Control.Alternative where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Control.Alt ((<|>))
import Control.Alternative (Alternative)
import Control.Plus (empty)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
-- | - Annihilation: `empty <*> x = empty`
checkAlternative :: forall f. (Alternative f, Arbitrary1 f, Eq1 f) => Proxy2 f -> QC Unit
checkAlternative _ = do

  log "Checking 'Left identity' law for Alternative"
  quickCheck distributivity

  log "Checking 'Annihilation' law for Alternative"
  quickCheck annihilation

  where

  distributivity :: Wrap f (A -> B) -> Wrap f (A -> B) -> Wrap f A -> Boolean
  distributivity (Wrap f) (Wrap g) (Wrap x) = ((f <|> g) <*> x) `eq1` ((f <*> x) <|> (g <*> x))

  annihilation :: Wrap f A -> Boolean
  annihilation (Wrap x) = (empty <*> x) `eq1` (empty :: f A)
