module Test.QuickCheck.Laws.Control.Category where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (B, C)
import Type.Proxy (Proxy3)

-- | - Identity: `id <<< p = p <<< id = p`
checkCategory
  ∷ ∀ eff a
  . Category a
  ⇒ Arbitrary (a B C)
  ⇒ Eq (a B C)
  ⇒ Proxy3 a
  → QC eff Unit
checkCategory _ = checkCategoryGen (arbitrary :: Gen (a B C))

checkCategoryGen
  ∷ ∀ eff a
  . Category a
  ⇒ Arbitrary (a B C)
  ⇒ Eq (a B C)
  ⇒ Gen (a B C)
  → QC eff Unit
checkCategoryGen gen = do

  log "Checking 'Identity' law for Category"
  quickCheck' 1000 $ identity <$> gen

  where

  identity ∷ a B C → Boolean
  identity p = (id <<< p) == p
            && (p <<< id) == p
