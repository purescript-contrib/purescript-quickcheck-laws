module Test.QuickCheck.Laws.Data.BooleanAlgebra where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.BooleanAlgebra (tt)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Excluded middle: `a || not a = tt`
checkBooleanAlgebra
  ∷ ∀ eff a
  . Arbitrary a
  ⇒ BooleanAlgebra a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkBooleanAlgebra _ = checkBooleanAlgebraGen (arbitrary :: Gen a)

checkBooleanAlgebraGen
  ∷ ∀ eff a
  . BooleanAlgebra a
  ⇒ Eq a
  ⇒ Gen a
  → QC eff Unit
checkBooleanAlgebraGen gen = do

  log "Checking 'Excluded middle' law for BooleanAlgebra"
  quickCheck' 1000 $ excludedMiddle <$> gen

  where

  excludedMiddle ∷ a → Boolean
  excludedMiddle a = (a || not a) == tt
