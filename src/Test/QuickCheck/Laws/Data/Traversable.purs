module Test.QuickCheck.Laws.Data.Traversable (checkTraversable) where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Functor.Compose      (Compose(..))
import Data.Identity             (Identity(..))
import Data.Traversable          (Traversable, traverse)
import Test.QuickCheck           (QC(), quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary)
import Type.Proxy                (Proxy(), Proxy2())

-- | - Naturality: `t <<< traverse f = traverse (t <<< f)` (not tested because it is implied by parametricity)
-- | - Identity: `traverse Identity = Identity`
-- | - Composition: `traverse (Compose <<< map g <<< f) = Compose <<< map (traverse g) <<< traverse f`
checkTraversable :: forall t f g a b c.
                 ( Traversable t
                 , Applicative f
                 , Applicative g
                 , Arbitrary (t a)
                 , Arbitrary (a -> f b)
                 , Arbitrary (b -> g c)
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

    log "Checking 'Identity' law for Traversable"
    quickCheck identity

    log "Checking 'Composition' law for Traversable"
    quickCheck composition

    where

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
