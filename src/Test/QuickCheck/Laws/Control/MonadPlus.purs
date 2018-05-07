module Test.QuickCheck.Laws.Control.MonadPlus where

import Prelude

import Control.Alt ((<|>))
import Control.MonadPlus (class MonadPlus)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Laws (A, B)
import Type.Proxy (Proxy2)

-- | - Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
checkMonadPlus
  ∷ ∀ m
  . MonadPlus m
  ⇒ Arbitrary (m A)
  ⇒ Arbitrary (m B)
  ⇒ Eq (m B)
  ⇒ Proxy2 m
  → Effect Unit
checkMonadPlus _ = do

  log "Checking 'Distributivity' law for MonadPlus"
  quickCheck' 1000 distributivity

  where

  distributivity ∷ m A → m A → (A → m B) → Boolean
  distributivity x y f = ((x <|> y) >>= f) == ((x >>= f) <|> (y >>= f))
