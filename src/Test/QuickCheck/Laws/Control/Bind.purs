module Test.QuickCheck.Laws.Control.Bind where

import Console (log)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

-- | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
checkBind :: forall m a. (Bind m,
                          Arbitrary a,
                          Arbitrary (m a),
                          Coarbitrary a,
                          Eq (m a)) => Proxy2 m
                                    -> Proxy a
                                    -> QC Unit
checkBind _ _ = do

  log "Checking 'Associativity' law for Bind"
  quickCheck associativity

  where

  associativity :: m a -> (a -> m a) -> (a -> m a) -> Boolean
  associativity m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
