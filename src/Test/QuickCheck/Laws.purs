module Test.QuickCheck.Laws where

import Prelude

import Data.Monoid (Monoid)

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, Coarbitrary, coarbitrary)

newtype A = A Ordering

instance eqA :: Eq A where
  eq (A x) (A y) = eq x y

instance ordA :: Ord A where
  compare (A x) (A y) = compare x y

instance boundedA :: Bounded A where
  top = A top
  bottom = A bottom

instance semigroupA :: Semigroup A where
  append (A x) (A y) = A (x <> y)

instance monoidA :: Monoid A where
  mempty = A EQ

instance arbitraryA :: Arbitrary A where
  arbitrary = A <$> arbitrary

instance coarbitraryA :: Coarbitrary A where
  coarbitrary (A x) = coarbitrary x

newtype B = B Ordering

instance eqB :: Eq B where
  eq (B x) (B y) = eq x y

instance ordB :: Ord B where
  compare (B x) (B y) = compare x y

instance boundedB :: Bounded B where
  top = B top
  bottom = B bottom

instance semigroupB :: Semigroup B where
  append (B x) (B y) = B (x <> y)

instance monoidB :: Monoid B where
  mempty = B EQ

instance arbitraryB :: Arbitrary B where
  arbitrary = B <$> arbitrary

instance coarbitraryB :: Coarbitrary B where
  coarbitrary (B x) = coarbitrary x

newtype C = C Ordering

instance eqC :: Eq C where
  eq (C x) (C y) = eq x y

instance ordC :: Ord C where
  compare (C x) (C y) = compare x y

instance boundedC :: Bounded C where
  top = C top
  bottom = C bottom

instance semigroupC :: Semigroup C where
  append (C x) (C y) = C (x <> y)

instance monoidC :: Monoid C where
  mempty = C EQ

instance arbitraryC :: Arbitrary C where
  arbitrary = C <$> arbitrary

instance coarbitraryC :: Coarbitrary C where
  coarbitrary (C x) = coarbitrary x

newtype D = D Ordering

instance eqD :: Eq D where
  eq (D x) (D y) = eq x y

instance ordD :: Ord D where
  compare (D x) (D y) = compare x y

instance boundedD :: Bounded D where
  top = D top
  bottom = D bottom

instance semigroupD :: Semigroup D where
  append (D x) (D y) = D (x <> y)

instance monoidD :: Monoid D where
  mempty = D EQ

instance arbitraryD :: Arbitrary D where
  arbitrary = D <$> arbitrary

instance coarbitraryD :: Coarbitrary D where
  coarbitrary (D x) = coarbitrary x

newtype E = E Ordering

instance eqE :: Eq E where
  eq (E x) (E y) = eq x y

instance ordE :: Ord E where
  compare (E x) (E y) = compare x y

instance boundedE :: Bounded E where
  top = E top
  bottom = E bottom

instance semigroupE :: Semigroup E where
  append (E x) (E y) = E (x <> y)

instance arbitraryE :: Arbitrary E where
  arbitrary = E <$> arbitrary

instance coarbitraryE :: Coarbitrary E where
  coarbitrary (E x) = coarbitrary x
