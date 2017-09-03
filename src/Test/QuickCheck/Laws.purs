module Test.QuickCheck.Laws
  ( module Test.QuickCheck.Laws
  , module Test.QuickCheck
  ) where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Monoid (class Monoid)
import Test.QuickCheck (QC)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary)

checkLaws ∷ ∀ eff. String → QC eff Unit → QC eff Unit
checkLaws typeName laws = do
  log $ "\n\nChecking laws of " <> typeName <> " instances...\n"
  laws

newtype A = A Ordering

derive newtype instance arbitraryA ∷ Arbitrary A
derive newtype instance boundedA ∷ Bounded A
derive newtype instance boundedEnumA :: BoundedEnum A
derive newtype instance coarbitraryA ∷ Coarbitrary A
derive newtype instance enumA :: Enum A
derive newtype instance eqA ∷ Eq A
derive newtype instance ordA ∷ Ord A
derive newtype instance semigroupA ∷ Semigroup A
instance monoidA ∷ Monoid A where mempty = A EQ

newtype B = B Ordering

derive newtype instance arbitraryB ∷ Arbitrary B
derive newtype instance boundedB ∷ Bounded B
derive newtype instance boundedEnumB :: BoundedEnum B
derive newtype instance coarbitraryB ∷ Coarbitrary B
derive newtype instance enumB :: Enum B
derive newtype instance eqB ∷ Eq B
derive newtype instance ordB ∷ Ord B
derive newtype instance semigroupB ∷ Semigroup B
instance monoidB ∷ Monoid B where mempty = B EQ 

newtype C = C Ordering

derive newtype instance arbitraryC ∷ Arbitrary C
derive newtype instance boundedC ∷ Bounded C
derive newtype instance boundedEnumC :: BoundedEnum C
derive newtype instance coarbitraryC ∷ Coarbitrary C
derive newtype instance enumC ∷ Enum C
derive newtype instance eqC ∷ Eq C
derive newtype instance ordC ∷ Ord C
derive newtype instance semigroupC ∷ Semigroup C
instance monoidC ∷ Monoid C where mempty = C EQ
                                  
newtype D = D Ordering

derive newtype instance arbitraryD ∷ Arbitrary D
derive newtype instance boundedD ∷ Bounded D
derive newtype instance boundedEnumD :: BoundedEnum D
derive newtype instance coarbitraryD ∷ Coarbitrary D
derive newtype instance enumD ∷ Enum D
derive newtype instance eqD ∷ Eq D
derive newtype instance ordD ∷ Ord D
derive newtype instance semigroupD ∷ Semigroup D
instance monoidD ∷ Monoid D where mempty = D EQ
                                  
newtype E = E Ordering

derive newtype instance arbitraryE ∷ Arbitrary E
derive newtype instance boundedE ∷ Bounded E
derive newtype instance boundedEnumE :: BoundedEnum E
derive newtype instance coarbitraryE ∷ Coarbitrary E
derive newtype instance enumE ∷ Enum E
derive newtype instance eqE ∷ Eq E
derive newtype instance ordE ∷ Ord E
derive newtype instance semigroupE ∷ Semigroup E
instance monoidE ∷ Monoid E where mempty = E EQ                         

