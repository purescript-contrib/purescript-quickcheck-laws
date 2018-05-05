module Test.QuickCheck.Laws.Control.Plus where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus, empty)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Left identity: `empty <|> x == x`
-- | - Right identity: `x <|> empty == x`
-- | - Annihilation: `f <$> empty == empty`
checkPlus
  ∷ ∀ f
  . Plus f
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Proxy2 f
  → Effect Unit
checkPlus _ = do

  log "Checking 'Left identity' law for Plus"
  quickCheck' 1000 leftIdentity

  log "Checking 'Right identity' law for Plus"
  quickCheck' 1000 rightIdentity

  log "Checking 'Annihilation' law for Plus"
  quickCheck' 1000 annihilation

  where

  leftIdentity ∷ f A → Boolean
  leftIdentity x = (empty <|> x) == x

  rightIdentity ∷ f A → Boolean
  rightIdentity x = (x <|> empty) == x

  annihilation ∷ (A → B) → Boolean
  annihilation f = (f <$> empty) == empty ∷ f B
