module Test.QuickCheck.Laws.Control.MonadPlus where

import Console (log)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.MonadPlus (MonadPlus)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

-- | - Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
-- | - Annihilation: `empty >>= f = empty`
checkMonadPlus :: forall m a b. (MonadPlus m,
                                 Arbitrary (a -> m b),
                                 Arbitrary (m a),
                                 Eq (m b)) => Proxy2 m
                                           -> Proxy a
                                           -> Proxy b
                                           -> QC Unit
checkMonadPlus _ _ _ = do

  log "Checking 'Distributivity' law for MonadPlus"
  quickCheck distributivity

  log "Checking 'Annihilation' law for MonadPlus"
  quickCheck annihilation

  where

  distributivity :: m a -> m a -> (a -> m b) -> Boolean
  distributivity x y f = ((x <|> y) >>= f) == ((x >>= f) <|> (y >>= f))

  annihilation :: (a -> m b) -> Boolean
  annihilation f = (empty >>= f) == empty
