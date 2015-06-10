module Test.QuickCheck.Laws.Control.Alternative where
    
import Prelude
    
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Control.Alt ((<|>))
import Control.Alternative (Alternative)
import Control.Plus (empty)

import Test.QuickCheck (QC(..), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary, Coarbitrary)

import Type.Proxy (Proxy(), Proxy2())

-- | - Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
-- | - Annihilation: `empty <*> x = empty`
checkAlternative :: forall f a b. (Alternative f,
                                   Arbitrary a,
                                   Arbitrary (f (a -> b)),
                                   Arbitrary (f a),
                                   Eq (f a),
                                   Eq (f b)) => Proxy2 f
                                             -> Proxy a
                                             -> Proxy b
                                             -> QC Unit
checkAlternative _ _ _ = do

  log "Checking 'Left identity' law for Alternative"
  quickCheck distributivity

  log "Checking 'Annihilation' law for Alternative"
  quickCheck annihilation

  where

  distributivity :: f (a -> b) -> f (a -> b) -> f a -> Boolean
  distributivity f g x = ((f <|> g) <*> x) == ((f <*> x) <|> (g <*> x))

  annihilation :: f a -> Boolean
  annihilation x = empty <*> x == empty :: f a
