module Test.QuickCheck.Laws.Control.Plus where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff.Console (log)
import Control.Plus (class Plus, empty)
import Test.QuickCheck (QC, arbitrary, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Left identity: `empty <|> x == x`
-- | - Right identity: `x <|> empty == x`
-- | - Annihilation: `f <$> empty == empty`
checkPlus
  ∷ ∀ eff f
  . Plus f
  ⇒ Arbitrary (f A)
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Proxy2 f
  → QC eff Unit
checkPlus _ = checkPlusGen (arbitrary ∷ Gen (f A))

checkPlusGen
  ∷ ∀ eff f
  . Plus f
  ⇒ Eq (f A)
  ⇒ Eq (f B)
  ⇒ Gen (f A)
  → QC eff Unit
checkPlusGen gen = do

  log "Checking 'Left identity' law for Plus"
  quickCheck' 1000 $ leftIdentity <$> gen

  log "Checking 'Right identity' law for Plus"
  quickCheck' 1000 $ rightIdentity <$> gen

  log "Checking 'Annihilation' law for Plus"
  quickCheck' 1000 annihilation

  where

  leftIdentity ∷ f A → Boolean
  leftIdentity x = (empty <|> x) == x

  rightIdentity ∷ f A → Boolean
  rightIdentity x = (x <|> empty) == x

  annihilation ∷ (A → B) → Boolean
  annihilation f = f <$> empty == empty ∷ f B
