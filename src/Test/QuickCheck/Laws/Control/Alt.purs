module Test.QuickCheck.Laws.Control.Alt where

import Prelude

import Control.Alt (class Alt, (<|>))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
-- | - Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`
checkAlt
  ∷ ∀ f
  . Alt f
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Proxy2 f
  → Effect Unit
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
