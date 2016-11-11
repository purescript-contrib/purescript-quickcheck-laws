
module Test.QuickCheck.Laws.Data.Enum where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Enum (pred, succ, class Enum)
import Data.Maybe (Maybe(Nothing))
import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

checkEnum
  ∷ ∀ eff a
  . (Arbitrary a, Enum a, Ord a)
  ⇒ Proxy a
  → QC eff Unit
checkEnum _ = do

  log "Checking 'GT ordering' law for Enum"
  quickCheck' 1000 gtordering

  log "Checking 'LT ordering' law for Enum"
  quickCheck' 1000 ltordering

  log "Checking 'predsuccpred' law for BoundedEnum"
  quickCheck' 1000 predsuccpredLaw

  log "Checking 'succpredsucc' law for BoundedEnum"
  quickCheck' 1000 succpredsuccLaw
  
  where
    gtordering ∷ a → Boolean
    gtordering a = succ a == Nothing || succ a > pred a

    ltordering ∷ a → Boolean
    ltordering a = succ a == Nothing || pred a < succ a
  
    predsuccpredLaw :: a -> Boolean
    predsuccpredLaw a = (pred a >>= succ >>= pred) == pred a

    succpredsuccLaw :: a -> Boolean
    succpredsuccLaw a = (succ a >>= pred >>= succ) == succ a
    
