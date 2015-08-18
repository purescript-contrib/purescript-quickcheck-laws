module Test.QuickCheck.Laws.Control.Extend where

import Control.Monad.Eff.Console (log)
import Control.Extend (Extend, (<<=))
import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)
import Type.Proxy (Proxy(), Proxy2())

import Prelude

-- | - Associativity: `extend f <<< extend g = extend (f <<< extend g)`
checkExtend :: forall w a b c eff. (Extend w,
                                    Arbitrary b,
                                    Arbitrary c,
                                    Arbitrary (w a),
                                    Coarbitrary (w a),
                                    Coarbitrary (w b),
                                    Eq (w c)) => Proxy2 w
                                              -> Proxy a
                                              -> Proxy b
                                              -> Proxy c
                                              -> QC eff Unit
checkExtend _ _ _ _ = do

  log "Checking 'Associativity' law for Extend"
  quickCheck associativity

  where

  associativity :: (w b -> c) -> (w a -> b) -> w a -> Boolean
  associativity f g x = ((f <<=) <<< (g <<=) $ x) == (f <<< (g <<=) <<= x)
