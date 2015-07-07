module Test.QuickCheck.Laws.Data.Traversable (checkTraversable) where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Functor.Compose      (Compose(..))
import Data.Identity             (Identity(..))
import Data.Traversable          (Traversable, traverse)
import Test.QuickCheck           (QC(), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary)
import Type.Proxy                (Proxy(), Proxy2())

-- | - Naturality: `t <<< traverse f = traverse (t <<< f)`
-- | - Identity: `traverse Identity = Identity`
-- | - Composition: `traverse (Compose <<< map g <<< f) = Compose <<< map (traverse g) <<< traverse f`
checkTraversable :: forall t f g a b c.
                 ( Traversable t
                 , Applicative f
                 , Applicative g
                 , Arbitrary (t a)
                 , Arbitrary (a -> f b)
                 -- , Arbitrary (forall k. f k -> g k)
                 , Arbitrary (b -> g c)
                 -- , Eq (g (t b))
                 , Eq (t a)
                 , Eq ((Compose f g) (t c))
                 )
                 => Proxy2 t
                 -> Proxy2 f
                 -> Proxy2 g
                 -> Proxy a
                 -> Proxy b
                 -> Proxy c
                 -> QC Unit
checkTraversable _ _ _ _ _ _ = do

    -- log "Checking 'Naturality' law for Traversable"
    -- quickCheck naturality

    log "Checking 'Identity' law for Traversable"
    quickCheck identity

    log "Checking 'Composition' law for Traversable"
    quickCheck composition

    where

    {-
    naturality :: (forall k. f k -> g k) -> (a -> f b) -> t a -> Boolean
    naturality t f x = lhs == rhs
      where lhs :: g (t b)
            lhs = (t <<< traverse f) x
            rhs :: g (t b)
            rhs = traverse (t <<< f) x
    -}

    identity :: t a -> Boolean
    identity x = lhs == rhs
      where lhs :: Identity (t a)
            lhs = (traverse Identity) x
            rhs :: Identity (t a)
            rhs = Identity x

    composition :: (a -> f b) -> (b -> g c) -> t a -> Boolean
    composition f g x = lhs == rhs
      where lhs :: (Compose f g) (t c)
            lhs = (traverse (Compose <<< map g <<< f)) x
            rhs :: (Compose f g) (t c)
            rhs = (Compose <<< map (traverse g) <<< traverse f) x
