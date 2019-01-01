module Test.QuickCheck.Laws.Control.Apply where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy2)

-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
checkApply
  ∷ ∀ f
  . Apply f
  ⇒ Arbitrary (f A)
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f (B → C))
  ⇒ Eq (f C)
  ⇒ Proxy2 f
  → Effect Unit
checkApply _ = do

  log "Checking 'Associative composition' law for Apply"
  quickCheck' 1000 associativeComposition

  where

  associativeComposition ∷ f (B → C) → f (A → B) → f A → Boolean
  associativeComposition f g x = ((<<<) <$> f <*> g <*> x) == (f <*> (g <*> x))


-- | Like `checkApply`, but with better error reporting.
-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
checkApplyShow
  ∷ ∀ f
  . Apply f
  ⇒ Arbitrary (f A)
  ⇒ Arbitrary (f (A → B))
  ⇒ Arbitrary (f (B → C))
  ⇒ Eq (f C)
  ⇒ Show (f C)
  ⇒ Proxy2 f
  → Effect Unit
checkApplyShow _ = do

  log "Checking 'Associative composition' law for Apply"
  quickCheck' 1000 associativeComposition

  where

  associativeComposition ∷ f (B → C) → f (A → B) → f A → Result
  associativeComposition f g x = ((<<<) <$> f <*> g <*> x) === (f <*> (g <*> x))
