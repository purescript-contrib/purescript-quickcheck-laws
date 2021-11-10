
module Test.QuickCheck.Laws.Data.Enum where

import Prelude

import Data.Enum (pred, succ, class Enum)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

checkEnum
  ∷ ∀ a
  . Arbitrary a
  ⇒ Enum a
  ⇒ Ord a
  ⇒ Proxy a
  → Effect Unit
checkEnum _ = checkEnumGen (arbitrary :: Gen a)

checkEnumGen
  ∷ ∀ a
  . Enum a
  ⇒ Ord a
  ⇒ Gen a
  → Effect Unit
checkEnumGen gen = do
  log "Checking 'Successor' law for Enum"
  quickCheck' 1000 $ successor <$> gen

  log "Checking 'Predecessor' law for Enum"
  quickCheck' 1000 $ predecessor <$> gen

  log "Checking 'Succ retracts pred' law for Enum"
  quickCheck' 1000 $ succRetractsPred <$> gen

  log "Checking 'Pred retracts succ' law for Enum"
  quickCheck' 1000 $ predRetractsSucc <$> gen

  log "Checking 'Non-skipping succ' law for Enum"
  quickCheck' 1000 $ nonSkippingSucc <$> gen <*> gen

  log "Checking 'Non-skipping pred' law for Enum"
  quickCheck' 1000 $ nonSkippingPred <$> gen <*> gen

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
