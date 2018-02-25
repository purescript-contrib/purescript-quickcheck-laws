
module Test.QuickCheck.Laws.Data.BoundedEnum where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff.Console (log)
import Data.Array (replicate, foldl)
import Data.Enum (toEnum, Cardinality, cardinality, fromEnum, class BoundedEnum, pred, succ)
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)


-- | - succ: `succ bottom >>= succ >>= succ ... succ [cardinality - 1 times] = top`
-- | - pred: `pred top    >>= pred >>= pred ... pred [cardinality - 1 times] = bottom`
-- | - predsucc: `forall a > bottom: pred a >>= succ = Just a`
-- | - succpred: `forall a < top:  succ a >>= pred = Just a`
-- | - enumpred: `forall a > bottom: fromEnum <$> pred a = Just (fromEnum a - 1)`
-- | - enumsucc: `forall a < top:  fromEnum <$> succ a = Just (fromEnum a + 1)`
-- | - compare: `compare e1 e2 = compare (fromEnum e1) (fromEnum e2)`
-- | - tofromenum: toEnum (fromEnum a) = Just a

checkBoundedEnum
  ∷ ∀ eff a
  . Arbitrary a
  ⇒ BoundedEnum a
  ⇒ Ord a
  ⇒ Proxy a
  → QC eff Unit
checkBoundedEnum gen = checkBoundedEnumGen (arbitrary :: Gen a)

checkBoundedEnumGen
  ∷ ∀ eff a
  . BoundedEnum a
  ⇒ Ord a
  ⇒ Gen a
  → QC eff Unit
checkBoundedEnumGen gen = do

  log "Checking 'succ' law for BoundedEnum"
  quickCheck' 1 succLaw

  log "Checking 'pred' law for BoundedEnum"
  quickCheck' 1 predLaw

  log "Checking 'predsucc' law for BoundedEnum"
  quickCheck' 1000 $ predsuccLaw <$> gen

  log "Checking 'succpred' law for BoundedEnum"
  quickCheck' 1000 $ succpredLaw <$> gen

  log "Checking 'enumpred' law for BoundedEnum"
  quickCheck' 1000 $ enumpredLaw <$> gen

  log "Checking 'enumsucc' law for BoundedEnum"
  quickCheck' 1000 $ enumsuccLaw <$> gen

  log "Checking 'compare' law for BoundedEnum"
  quickCheck' 1000 $ lift2 compareLaw gen gen

  log "Checking 'tofromenum' law for BoundedEnum"
  quickCheck' 1000 $ tofromenumLaw <$> gen


  where
    c :: Int
    c = unwrap (cardinality :: Cardinality a)
    
    succLaw :: Boolean
    succLaw = (Just top :: Maybe a) ==
                foldl (>>=) (pure bottom) (replicate (c - 1) succ)

    predLaw :: Boolean
    predLaw = (Just bottom :: Maybe a) ==
                foldl (>>=) (pure top) (replicate (c - 1) pred)

    predsuccLaw :: a -> Boolean
    predsuccLaw a = a == bottom || (pred a >>= succ) == Just a
    
    succpredLaw :: a -> Boolean
    succpredLaw a = a == top || (succ a >>= pred) == Just a

    enumpredLaw :: a -> Boolean
    enumpredLaw a = a == bottom || (fromEnum <$> pred a) == Just (fromEnum a - 1)
    
    enumsuccLaw :: a -> Boolean
    enumsuccLaw a = a == top || (fromEnum <$> succ a) == Just (fromEnum a + 1)

    compareLaw :: a -> a -> Boolean
    compareLaw a b = a `compare` b == fromEnum a `compare` fromEnum b

    tofromenumLaw :: a -> Boolean
    tofromenumLaw a = toEnum (fromEnum a) == Just a
