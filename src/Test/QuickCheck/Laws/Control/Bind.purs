module Test.QuickCheck.Laws.Control.Bind where

import Debug.Trace (trace)
import Test.QuickCheck (QC(..), Arbitrary, CoArbitrary, quickCheck)

-- | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
checkBind :: forall m a. (Bind m,
                          Arbitrary a,
                          Arbitrary (m a),
                          CoArbitrary a,
                          Eq (m a)) => m a -> QC Unit
checkBind _ = do

  trace "Checking 'Associativity' law for Bind"
  quickCheck associativity

  where

  associativity :: m a -> (a -> m a) -> (a -> m a) -> Boolean
  associativity m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
