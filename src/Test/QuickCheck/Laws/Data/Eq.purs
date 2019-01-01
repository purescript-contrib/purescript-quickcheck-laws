module Test.QuickCheck.Laws.Data.Eq where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck', Result, (===), (/==))
import Test.QuickCheck.Combinators ((==>), (&=&), not')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- | - Negation: `x /= y = not (x == y)`
checkEq
  ∷ ∀ a
  . Arbitrary a
  ⇒ Eq a
  ⇒ Proxy a
  → Effect Unit
checkEq _ = do

  log "Checking 'Reflexivity' law for Eq"
  quickCheck' 1000 reflexivity

  log "Checking 'Symmetry' law for Eq"
  quickCheck' 1000 symmetry

  log "Checking 'Transitivity' law for Eq"
  quickCheck' 1000 transitivity

  log "Checking 'Negation' law for Eq"
  quickCheck' 1000 negation

  where

  reflexivity ∷ a → Boolean
  reflexivity x = (x == x) == true

  symmetry ∷ a → a → Boolean
  symmetry x y = (x == y) == (y == x)

  transitivity ∷ a → a → a → Boolean
  transitivity x y z = if (x == y) && (y == z) then x == z else true

  negation ∷ a → a → Boolean
  negation x y = (x /= y) == not (x == y)


-- | Like `checkEq`, with better error reporting.
-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- | - Negation: `x /= y = not (x == y)`
checkEqShow
  ∷ ∀ a
  . Arbitrary a
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → Effect Unit
checkEqShow _ = do

  log "Checking 'Reflexivity' law for Eq"
  quickCheck' 1000 reflexivity

  log "Checking 'Symmetry' law for Eq"
  quickCheck' 1000 symmetry

  log "Checking 'Transitivity' law for Eq"
  quickCheck' 1000 transitivity

  log "Checking 'Negation' law for Eq"
  quickCheck' 1000 negation

  where

  reflexivity ∷ a → Result
  reflexivity x = x === x

  symmetry ∷ a → a → Result
  symmetry x y = ((x === y) ==> (y === x)) &=& ((x /== y) ==> (y /== x))

  transitivity ∷ a → a → a → Result
  transitivity x y z = ((x === y) &=& (y === z)) ==> (x === z)

  negation ∷ a → a → Result
  negation x y = (x /== y) ==> not' "Shouldn't be equal" (x === y)
