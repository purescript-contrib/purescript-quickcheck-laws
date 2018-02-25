module Test.QuickCheck.Laws.Data.EuclideanRing where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Integral domain: `a /= 0` and `b /= 0` implies `a * b /= 0`
-- | - Multiplicative Euclidean function: ``a = (a / b) * b + (a `mod` b)``
-- |   where `degree a > 0` and `degree a <= degree (a * b)`
checkEuclideanRing
  ∷ ∀ eff a
  . EuclideanRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → QC eff Unit
checkEuclideanRing _ = checkEuclideanRingGen (arbitrary :: Gen a)

checkEuclideanRingGen
  ∷ ∀ eff a
  . EuclideanRing a
  ⇒ Eq a
  ⇒ Gen a
  → QC eff Unit
checkEuclideanRingGen gen = do

  log "Checking 'Integral domain' law for EuclideanRing"
  log "one /= zero:"
  quickCheck' 1 \(_ :: Unit) -> (zero /= one :: a)
  log "product of nonzero elements is nonzero:"
  quickCheck' 1000 $ lift2 integralDomain gen gen

  log "Checking 'Nonnegative euclidean function' law for EuclideanRing"
  quickCheck' 1000 $ nonnegativeEuclideanFunc <$> gen

  log "Checking 'Quotient/remainder' law for EuclideanRing"
  quickCheck' 1000 $ lift2 quotRem gen gen

  log "Checking 'Submultiplicative euclidean function' law for EuclideanRing"
  quickCheck' 1000 $ lift2 submultiplicative gen gen

  where

  integralDomain ∷ a → a → Boolean
  integralDomain a b
    | a /= zero && b /= zero = a * b /= zero
    | otherwise = true

  nonnegativeEuclideanFunc ∷ a → Boolean
  nonnegativeEuclideanFunc a
    | a /= zero = degree a >= zero
    | otherwise = true

  quotRem ∷ a → a → Boolean
  quotRem a b
    | b /= zero =
        let
          q = a / b
          r = a `mod` b
        in
          a == q*b + r && (r == zero || degree r < degree b)

    | otherwise = true

  submultiplicative ∷ a → a → Boolean
  submultiplicative a b
    | a /= zero && b /= zero = degree a <= degree (a * b)
    | otherwise = true
