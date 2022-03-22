module Test.QuickCheck.Laws.Control.Category where

import Prelude

import Data.Function as F
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (B, C)
import Type.Proxy (Proxy)

-- | - Identity: `id <<< p = p <<< id = p`
checkCategory
  ∷ ∀ a
  . Category a
  ⇒ Arbitrary (a B C)
  ⇒ Eq (a B C)
  ⇒ Proxy a
  → Effect Unit
checkCategory _ = checkCategoryGen (arbitrary :: Gen (a B C))

checkCategoryGen
  ∷ ∀ a
  . Category a
  ⇒ Arbitrary (a B C)
  ⇒ Eq (a B C)
  ⇒ Gen (a B C)
  → Effect Unit
checkCategoryGen gen = do
  log "Checking 'Identity' law for Category"
  quickCheck' 1000 $ identity <$> gen

  where

  identity ∷ a B C → Boolean
  identity p = (F.identity <<< p) == p
            && (p <<< F.identity) == p
