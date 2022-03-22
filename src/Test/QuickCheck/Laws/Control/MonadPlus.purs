module Test.QuickCheck.Laws.Control.MonadPlus where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift3)
import Control.MonadPlus (class MonadPlus)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy)

-- | - Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
checkMonadPlus
  ∷ ∀ m
  . MonadPlus m
  ⇒ Arbitrary (m A)
  ⇒ Arbitrary (m B)
  ⇒ Eq (m B)
  ⇒ Proxy m
  → Effect Unit
checkMonadPlus _ =
  checkMonadPlusGen
    (arbitrary :: Gen (m A))
    (arbitrary :: Gen (A → m B))

checkMonadPlusGen
  ∷ ∀ m
  . MonadPlus m
  ⇒ Eq (m B)
  ⇒ Gen (m A)
  → Gen (A → m B)
  → Effect Unit
checkMonadPlusGen gen genf = do
  log "Checking 'Distributivity' law for MonadPlus"
  quickCheck' 1000 $ lift3 distributivity gen gen genf

  where

  distributivity ∷ m A → m A → (A → m B) → Boolean
  distributivity x y f = ((x <|> y) >>= f) == ((x >>= f) <|> (y >>= f))
