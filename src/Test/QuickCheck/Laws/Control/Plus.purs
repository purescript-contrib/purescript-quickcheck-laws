module Test.QuickCheck.Laws.Control.Plus where

import Console (log)
import Control.Alt ((<|>))
import Control.Plus (Plus, empty)
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

-- | - Left identity: `empty <|> x == x`
-- | - Right identity: `x <|> empty == x`
-- | - Annihilation: `f <$> empty == empty`
checkPlus :: forall f a b. (Plus f,
                           Arbitrary a,
                           Arbitrary (a -> b),
                           Arbitrary (f a),
                           Eq (f a),
                           Eq (f b)) => Proxy2 f
                                     -> Proxy a
                                     -> Proxy b
                                     -> QC Unit
checkPlus _ _ _ = do

  log "Checking 'Left identity' law for Plus"
  quickCheck leftIdentity

  log "Checking 'Right identity' law for Plus"
  quickCheck rightIdentity

  log "Checking 'Annihilation' law for Plus"
  quickCheck annihilation

  where

  leftIdentity :: f a -> Boolean
  leftIdentity x = (empty <|> x) == x

  rightIdentity :: f a -> Boolean
  rightIdentity x = (x <|> empty) == x

  annihilation :: (a -> b) -> Boolean
  annihilation f = f <$> empty == empty :: f b
