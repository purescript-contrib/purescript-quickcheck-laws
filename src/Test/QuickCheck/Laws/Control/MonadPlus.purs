module Test.QuickCheck.Laws.Control.MonadPlus where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift3)
import Control.Monad.Eff.Console (log)
import Control.MonadPlus (class MonadPlus)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
checkMonadPlus
  ∷ ∀ eff m
  . MonadPlus m
  ⇒ Arbitrary (m A)
  ⇒ Arbitrary (m B)
  ⇒ Eq (m B)
  ⇒ Proxy2 m
  → QC eff Unit
checkMonadPlus _ =
  checkMonadPlusGen
    (arbitrary :: Gen (m A))
    (arbitrary :: Gen (A → m B))

checkMonadPlusGen
  ∷ ∀ eff m
  . MonadPlus m
  ⇒ Eq (m B)
  ⇒ Gen (m A)
  → Gen (A → m B)
  → QC eff Unit
checkMonadPlusGen gen genf = do

  log "Checking 'Distributivity' law for MonadPlus"
  quickCheck' 1000 $ lift3 distributivity gen gen genf

  where

  distributivity ∷ m A → m A → (A → m B) → Boolean
  distributivity x y f = ((x <|> y) >>= f) == ((x >>= f) <|> (y >>= f))
