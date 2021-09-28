module Test.QuickCheck.Laws.Data.EuclideanRing where

import Prelude

import Control.Apply (lift2)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy)

-- | - Integral domain: `one /= zero`, and if `a` and `b` are both nonzero then
-- |   so is their product `a * b`
-- | - Euclidean function `degree`:
-- |   - Nonnegativity: For all nonzero `a`, `degree a >= 0`
-- |   - Quotient/remainder: For all `a` and `b`, where `b` is nonzero,
-- |     let `q = a / b` and ``r = a `mod` b``; then `a = q*b + r`, and also
-- |     either `r = zero` or `degree r < degree b`
-- | - Submultiplicative euclidean function:
-- |   - For all nonzero `a` and `b`, `degree a <= degree (a * b)`
checkEuclideanRing
  ∷ ∀ a
  . EuclideanRing a
  ⇒ Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkEuclideanRing _ = checkEuclideanRingGen (arbitrary :: Gen a)

checkEuclideanRingGen
  ∷ ∀ a
  . EuclideanRing a
  ⇒ Eq a
  ⇒ Gen a
  → Effect Unit
checkEuclideanRingGen gen = do
  log "Checking 'Integral domain' law for EuclideanRing"
  log "one /= zero:"
  quickCheck' 1 \(_ :: Unit) -> (zero /= (one :: a))
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
    | a == zero || b == zero = true
    | otherwise = degree a <= degree (a * b)
