module Test.QuickCheck.Laws.Data.EuclideanRing where

import Prelude

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy)

import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)

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
checkEuclideanRing _ = do

  log "Checking 'Integral domain' law for EuclideanRing"
  log "one /= zero:"
  quickCheck' 1 \(_ :: Unit) -> (zero /= one :: a)
  log "product of nonzero elements is nonzero:"
  quickCheck' 1000 integralDomain

  log "Checking 'Nonnegative euclidean function' law for EuclideanRing"
  quickCheck' 1000 nonnegativeEuclideanFunc

  log "Checking 'Quotient/remainder' law for EuclideanRing"
  quickCheck' 1000 quotRem

  log "Checking 'Submultiplicative euclidean function' law for EuclideanRing"
  quickCheck' 1000 submultiplicative

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
    | a == zero || b == zero = true
    | productOverflows a b = true
    | otherwise = degree a <= degree (a * b)
                  
  productOverflows :: a -> a -> Boolean
  productOverflows a b = a * b / b /= a
