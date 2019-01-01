module Test.QuickCheck.Laws.Control.Category where

import Prelude

import Data.Function as F
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Combinators ((&=&))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (B, C)
import Type.Proxy (Proxy3)

-- | - Identity: `id <<< p = p <<< id = p`
checkCategory
  ∷ ∀ a
  . Category a
  ⇒ Arbitrary (a B C)
  ⇒ Eq (a B C)
  ⇒ Proxy3 a
  → Effect Unit
checkCategory _ = do

  log "Checking 'Identity' law for Category"
  quickCheck' 1000 identity

  where

  identity ∷ a B C → Boolean
  identity p = (F.identity <<< p) == p
            && (p <<< F.identity) == p


-- | - Identity: `id <<< p = p <<< id = p`
checkCategoryShow
  ∷ ∀ a
  . Category a
  ⇒ Arbitrary (a B C)
  ⇒ Eq (a B C)
  ⇒ Show (a B C)
  ⇒ Proxy3 a
  → Effect Unit
checkCategoryShow _ = do

  log "Checking 'Identity' law for Category"
  quickCheck' 1000 identity

  where

  identity ∷ a B C → Result
  identity p = ((F.identity <<< p) === p)
           &=& ((p <<< F.identity) === p)
