module Test.QuickCheck.Laws.Control.Alternative where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift3)
import Control.Plus (empty)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy)

-- | - Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
-- | - Annihilation: `empty <*> x = empty`
checkAlternative
  ∷ ∀ f
  . Alternative f
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Proxy f
  → Effect Unit
checkAlternative =
  checkAlternativeGen (arbitrary :: Gen (f A)) (arbitrary :: Gen (f (A → B)))

checkAlternativeGen
  ∷ ∀ f
  . Alternative f
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Gen (f A)
  → Gen (f (A → B))
  → Proxy f
  → Effect Unit
checkAlternativeGen gen genf _ = do
  log "Checking 'Left identity' law for Alternative"
  quickCheck' 1000 $ lift3 distributivity genf genf gen

  log "Checking 'Annihilation' law for Alternative"
  quickCheck' 1000 $ annihilation <$> gen

  where

  distributivity ∷ f (A → B) → f (A → B) → f A → Boolean
  distributivity f g x = ((f <|> g) <*> x) == ((f <*> x) <|> (g <*> x))

  annihilation ∷ f A → Boolean
  annihilation x = (empty <*> x) == (empty ∷ f A)
