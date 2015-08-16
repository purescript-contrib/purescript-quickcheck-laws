module Test.QuickCheck.Laws.Control.Comonad where

import Control.Monad.Eff.Console (log)
import Control.Comonad (Comonad, extract)
import Control.Extend ((<<=))
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

import Prelude

-- | - Left Identity: `extract <<= x = x`
-- | - Right Identity: `extract (f <<= x) = f x`
checkComonad :: forall w a b. (Comonad w,
                              Arbitrary a,
                              Arbitrary b,
                              Arbitrary (w a),
                              Coarbitrary (w a),
                              Eq b,
                              Eq (w a)) => Proxy2 w
                                        -> Proxy a
                                        -> Proxy b
                                        -> QC Unit
checkComonad _ _ _ = do

  log "Checking 'Left identity' law for Comonad"
  quickCheck leftIdentity

  log "Checking 'Right identity' law for Comonad"
  quickCheck rightIdentity

  where

  leftIdentity :: w a -> Boolean
  leftIdentity x = (extract <<= x) == x

  rightIdentity :: (w a -> b) -> w a -> Boolean
  rightIdentity f x = extract (f <<= x) == f x
