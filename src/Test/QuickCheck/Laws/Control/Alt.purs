module Test.QuickCheck.Laws.Control.Alt where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Control.Alt (Alt, (<|>))

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Test.QuickCheck.Laws

import Type.Proxy (Proxy(), Proxy2())

-- | - Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
-- | - Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`
checkAlt :: forall f. (Alt f, Arbitrary1 f, Eq1 f) => Proxy2 f -> QC Unit
checkAlt _ = do

  log "Checking 'Associativity' law for Alt"
  quickCheck associativity

  log "Checking 'Distributivity' law for Alt"
  quickCheck distributivity

  where

  associativity :: Wrap f A -> Wrap f A -> Wrap f A -> Boolean
  associativity (Wrap x) (Wrap y) (Wrap z) = ((x <|> y) <|> z) `eq1` (x <|> (y <|> z))

  distributivity :: (A -> B) -> Wrap f A -> Wrap f A -> Boolean
  distributivity f (Wrap x) (Wrap y) = (f <$> (x <|> y)) `eq1` ((f <$> x) <|> (f <$> y))
