module Test.QuickCheck.Laws.Control.Alternative where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift3)
import Control.Monad.Eff.Console (log)
import Control.Plus (empty)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
-- | - Annihilation: `empty <*> x = empty`
checkAlternative
  ∷ ∀ eff f
  . Alternative f
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Proxy2 f
  → QC eff Unit
checkAlternative _ =
  checkAlternativeGen (arbitrary :: Gen (f A)) (arbitrary :: Gen (f (A → B)))

checkAlternativeGen
  ∷ ∀ eff f
  . Alternative f
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Gen (f A)
  → Gen (f (A → B))
  → QC eff Unit
checkAlternativeGen gen genf = do

  log "Checking 'Left identity' law for Alternative"
  quickCheck' 1000 $ lift3 distributivity genf genf gen

  log "Checking 'Annihilation' law for Alternative"
  quickCheck' 1000 $ annihilation <$> gen

  where

  distributivity ∷ f (A → B) → f (A → B) → f A → Boolean
  distributivity f g x = ((f <|> g) <*> x) == ((f <*> x) <|> (g <*> x))

  annihilation ∷ f A → Boolean
  annihilation x = empty <*> x == empty ∷ f A
