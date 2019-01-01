module Test.QuickCheck.Laws.Data.BooleanAlgebra where

import Prelude

import Data.BooleanAlgebra (tt)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

-- | - Excluded middle: `a || not a = tt`
checkBooleanAlgebra
  ∷ ∀ a
  . Arbitrary a
  ⇒ BooleanAlgebra a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkBooleanAlgebra _ = do

  log "Checking 'Excluded middle' law for BooleanAlgebra"
  quickCheck' 1000 excludedMiddle

  where

  excludedMiddle ∷ a → Boolean
  excludedMiddle a = (a || not a) == tt



-- | - Excluded middle: `a || not a = tt`, with better error reporting
checkBooleanAlgebraShow
  ∷ ∀ a
  . Arbitrary a
  ⇒ BooleanAlgebra a
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → Effect Unit
checkBooleanAlgebraShow _ = do

  log "Checking 'Excluded middle' law for BooleanAlgebra"
  quickCheck' 1000 excludedMiddle

  where

  excludedMiddle ∷ a → Result
  excludedMiddle a = (a || not a) === tt
