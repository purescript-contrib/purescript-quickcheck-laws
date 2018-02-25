module Test.QuickCheck.Laws.Control.Apply where

import Prelude

import Control.Apply (lift3)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy2)

-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
checkApply
  ∷ ∀ eff f
  . Apply f
  ⇒ Arbitrary (f A)
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f (B → C))
  ⇒ Eq (f C)
  ⇒ Proxy2 f
  → QC eff Unit
checkApply _ =
  checkApplyGen
    (arbitrary :: Gen (f A))
    (arbitrary :: Gen (f (A → B)))
    (arbitrary :: Gen (f (B → C)))

checkApplyGen
  ∷ ∀ eff f
  . Apply f
  ⇒ Eq (f C)
  ⇒ Gen (f A)
  → Gen (f (A → B))
  → Gen (f (B → C))
  → QC eff Unit
checkApplyGen gen genab genbc = do

  log "Checking 'Associative composition' law for Apply"
  quickCheck' 1000 $ lift3 associativeComposition genbc genab gen

  where

  associativeComposition ∷ f (B → C) → f (A → B) → f A → Boolean
  associativeComposition f g x = ((<<<) <$> f <*> g <*> x) == (f <*> (g <*> x))
