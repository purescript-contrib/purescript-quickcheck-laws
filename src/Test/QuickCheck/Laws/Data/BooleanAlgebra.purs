module Test.QuickCheck.Laws.Data.BooleanAlgebra where

import Prelude

import Data.BooleanAlgebra (tt)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Excluded middle: `a || not a = tt`
checkBooleanAlgebra
  ∷ ∀ a
  . Arbitrary a
  ⇒ BooleanAlgebra a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkBooleanAlgebra _ = checkBooleanAlgebraGen (arbitrary :: Gen a)

checkBooleanAlgebraGen
  ∷ ∀ a
  . BooleanAlgebra a
  ⇒ Eq a
  ⇒ Gen a
  → Effect Unit
checkBooleanAlgebraGen gen = do
  log "Checking 'Excluded middle' law for BooleanAlgebra"
  quickCheck' 1000 $ excludedMiddle <$> gen

  where

  excludedMiddle ∷ a → Boolean
  excludedMiddle a = (a || not a) == tt
