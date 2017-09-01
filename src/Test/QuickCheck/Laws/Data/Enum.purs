
module Test.QuickCheck.Laws.Data.Enum where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Enum (pred, succ, class Enum)
import Data.Maybe (maybe)
import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

checkEnum
  ∷ ∀ eff a
  . Arbitrary a
  ⇒ Enum a
  ⇒ Ord a
  ⇒ Proxy a
  → QC eff Unit
checkEnum _ = do


  log "Checking 'Successor' law for Enum"
  quickCheck' 1000 successor

  log "Checking 'Predecessor' law for Enum"
  quickCheck' 1000 predecessor

  log "Checking 'Succ retracts pred' law for Enum"
  quickCheck' 1000 succRetractsPred

  log "Checking 'Pred retracts succ' law for Enum"
  quickCheck' 1000 predRetractsSucc

  log "Checking 'Non-skipping succ' law for Enum"
  quickCheck' 1000 nonSkippingSucc

  log "Checking 'Non-skipping pred' law for Enum"
  quickCheck' 1000 nonSkippingPred


  where

    successor :: a -> Boolean
    successor a = maybe true (a < _) (succ a)

    predecessor :: a -> Boolean
    predecessor a = maybe true (_ < a) (pred a)

    succRetractsPred :: a -> Boolean
    succRetractsPred a = (pred a >>= succ >>= pred) == pred a

    predRetractsSucc :: a -> Boolean
    predRetractsSucc a = (succ a >>= pred >>= succ) == succ a

    nonSkippingSucc :: a -> a -> Boolean
    nonSkippingSucc a b = b <= a || maybe false (_ <= b) (succ a)

    nonSkippingPred :: a -> a -> Boolean
    nonSkippingPred a b = a <= b || maybe false (b <= _) (pred a)

