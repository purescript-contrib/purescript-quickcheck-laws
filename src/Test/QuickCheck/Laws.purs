module Test.QuickCheck.Laws
  ( Arbitrary1
  , arbitrary1
  , Coarbitrary1
  , coarbitrary1
  , Eq1
  , eq1
  , Wrap(..)
  , unwrap
  , A()
  , B()
  , C()
  ) where
    
import Prelude
    
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
    
-- | A version of `Arbitrary` for type constructors
class Arbitrary1 f where
  arbitrary1 :: forall a. (Arbitrary a) => Gen (f a)
    
-- | A version of `Coarbitrary` for type constructors
class Coarbitrary1 f where
  coarbitrary1 :: forall a b. (Coarbitrary a) => f a -> Gen b -> Gen b
  
data Wrap f a = Wrap (f a)

unwrap :: forall f a. Wrap f a -> f a
unwrap (Wrap wrapped) = wrapped

instance arbitraryWrap :: (Arbitrary1 f, Arbitrary a) => Arbitrary (Wrap f a) where
  arbitrary = Wrap <$> arbitrary1

instance coarbitraryWrap :: (Coarbitrary1 f, Coarbitrary a) => Coarbitrary (Wrap f a) where
  coarbitrary = coarbitrary1 <<< unwrap

instance eqWrap :: (Eq1 f, Eq a) => Eq (Wrap f a) where
  eq (Wrap a) (Wrap b) = eq1 a b
    
-- | A version of `Eq` for type constructors
class Eq1 f where
  eq1 :: forall a. (Eq a) => f a -> f a -> Boolean
  
-- | A type used to instantiate type constructors. 
data A = A Int

type B = A

type C = A

unA :: A -> Int
unA (A a) = a

instance eqA :: Eq A where
  eq (A a) (A b) = a == b
  
instance arbA :: Arbitrary A where
  arbitrary = A <$> chooseInt 0 10
  
instance coarbA :: Coarbitrary A where
  coarbitrary = coarbitrary <<< unA