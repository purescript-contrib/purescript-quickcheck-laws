module Test.QuickCheck.Laws.Control.Alt where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2, lift3)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy)

-- | - Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
-- | - Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`
checkAlt
  ∷ ∀ f
  . Alt f
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Proxy f
  → Effect Unit
checkAlt _ = checkAltGen (arbitrary :: Gen (f A))

checkAltGen
  ∷ ∀ f
  . Alt f
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Gen (f A)
  → Effect Unit
checkAltGen gen = do
  log "Checking 'Associativity' law for Alt"
  quickCheck' 1000 $ lift3 associativity gen gen gen

  log "Checking 'Distributivity' law for Alt"
  quickCheck' 1000 $ lift2 distributivity gen gen

  where

  associativity ∷ f A → f A → f A → Boolean
  associativity x y z = ((x <|> y) <|> z) == (x <|> (y <|> z))

  distributivity ∷ f A → f A → (A → B) → Boolean
  distributivity x y f = (f <$> (x <|> y)) == ((f <$> x) <|> (f <$> y))
