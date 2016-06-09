module Test.QuickCheck.Laws.Control.Alternative where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Monad.Eff.Console (log)
import Control.Plus (empty)

import Type.Proxy (Proxy2())

import Test.QuickCheck (QC(), quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A(), B())

-- | - Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
-- | - Annihilation: `empty <*> x = empty`
checkAlternative :: forall eff f. (Alternative f, Arbitrary (f (A -> B)), Arbitrary (f A), Eq (f A), Eq (f B)) => Proxy2 f -> QC eff Unit
checkAlternative _ = do

  log "Checking 'Left identity' law for Alternative"
  quickCheck' 1000 distributivity

  log "Checking 'Annihilation' law for Alternative"
  quickCheck' 1000 annihilation

  where

  distributivity :: f (A -> B) -> f (A -> B) -> f A -> Boolean
  distributivity f g x = ((f <|> g) <*> x) == ((f <*> x) <|> (g <*> x))

  annihilation :: f A -> Boolean
  annihilation x = empty <*> x == empty :: f A
