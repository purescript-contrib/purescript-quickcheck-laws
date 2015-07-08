module Test.QuickCheck.Laws.Control.Alt where

import Control.Monad.Eff.Console (log)
import Control.Alt (Alt, (<|>))
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

import Prelude

-- | - Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
-- | - Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`
checkAlt :: forall f a b. (Alt f,
                           Arbitrary a,
                           Arbitrary b,
                           Arbitrary (f a),
                           Coarbitrary a,
                           Eq (f a),
                           Eq (f b)) => Proxy2 f
                                     -> Proxy a
                                     -> Proxy b
                                     -> QC Unit
checkAlt _ _ _ = do

  log "Checking 'Associativity' law for Alt"
  quickCheck associativity

  log "Checking 'Distributivity' law for Alt"
  quickCheck distributivity

  where

  associativity :: f a -> f a -> f a -> Boolean
  associativity x y z = ((x <|> y) <|> z) == (x <|> (y <|> z))

  distributivity :: (a -> b) -> f a -> f a -> Boolean
  distributivity f x y = (f <$> (x <|> y)) == ((f <$> x) <|> (f <$> y))
