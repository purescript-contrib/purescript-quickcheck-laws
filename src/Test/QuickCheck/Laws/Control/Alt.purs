module Test.QuickCheck.Laws.Control.Alt where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy2)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)

-- | - Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
-- | - Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`
checkAlt
  ∷ ∀ eff f
  . (Alt f, Arbitrary (f A), Eq (f A), Eq (f B))
  ⇒ Proxy2 f
  → QC eff Unit
checkAlt _ = do

  log "Checking 'Associativity' law for Alt"
  quickCheck' 1000 associativity

  log "Checking 'Distributivity' law for Alt"
  quickCheck' 1000 distributivity

  where

  associativity ∷ f A → f A → f A → Boolean
  associativity x y z = ((x <|> y) <|> z) == (x <|> (y <|> z))

  distributivity ∷ (A → B) → f A → f A → Boolean
  distributivity f x y = (f <$> (x <|> y)) == ((f <$> x) <|> (f <$> y))
