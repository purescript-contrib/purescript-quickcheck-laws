# Module Documentation

## Module Test.QuickCheck.Classes.Applicative

#### `checkApplicative`

``` purescript
checkApplicative :: forall f a b c. (Applicative f, Arbitrary a, Arbitrary b, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), CoArbitrary a, Eq (f a), Eq (f b), Eq (f c)) => f a -> f b -> f c -> QC Unit
```

- Identity: `(pure id) <*> v = v`
- Composition: `(pure (<<<)) <*> f <*> g <*> h = f <*> (g <*> h)`
- Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
- Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`


## Module Test.QuickCheck.Laws.Apply

#### `checkApply`

``` purescript
checkApply :: forall f a b c. (Apply f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Eq (f c)) => f a -> f b -> f c -> QC Unit
```

- Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`


## Module Test.QuickCheck.Laws.Bind

#### `checkBind`

``` purescript
checkBind :: forall m a. (Bind m, Arbitrary a, Arbitrary (m a), CoArbitrary a, Eq (m a)) => m a -> QC Unit
```

- Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`


## Module Test.QuickCheck.Laws.Bounded

#### `checkBounded`

``` purescript
checkBounded :: forall m a. (Arbitrary a, Bounded a) => a -> QC Unit
```

- Ordering: `bottom <= a <= top`


## Module Test.QuickCheck.Laws.BoundedLattice

#### `checkBoundedLattice`

``` purescript
checkBoundedLattice :: forall m a. (Arbitrary a, BoundedLattice a) => a -> QC Unit
```

- Identity:
  - `a || bottom = a`
  - `a && top = a`
- Annihiliation:
  - `a || top = top`
  - `a && bottom = bottom`


## Module Test.QuickCheck.Laws.Category

#### `checkCategory`

``` purescript
checkCategory :: forall a b. (Category a, Arbitrary (a b b), Eq (a b b)) => a b b -> QC Unit
```

- Identity: `id <<< p = p <<< id = p`


## Module Test.QuickCheck.Laws.ComplementedLattice

#### `checkComplementedLattice`

``` purescript
checkComplementedLattice :: forall m a. (Arbitrary a, ComplementedLattice a) => a -> QC Unit
```

- Complemented:
  - `not a || a == top`
  - `not a && a == bottom`
- Double negation:
  - `not <<< not == id`


## Module Test.QuickCheck.Laws.DistributiveLattice

#### `checkDistributiveLattice`

``` purescript
checkDistributiveLattice :: forall m a. (Arbitrary a, DistributiveLattice a) => a -> QC Unit
```

- Distributivity: `x && (y || z) = (x && y) || (x && z)`


## Module Test.QuickCheck.Laws.DivisionRing

#### `checkDivisionRing`

``` purescript
checkDivisionRing :: forall a. (DivisionRing a, Arbitrary a, Eq a) => a -> QC Unit
```

- Multiplicative inverse: `(one / x) * x = one`


## Module Test.QuickCheck.Laws.Eq

#### `checkEq`

``` purescript
checkEq :: forall m a. (Arbitrary a, Eq a) => a -> QC Unit
```

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`
- Negation: `x /= y = not (x == y)`


## Module Test.QuickCheck.Laws.Functor

#### `checkFunctor`

``` purescript
checkFunctor :: forall f a b c. (Functor f, Arbitrary a, Arbitrary (f a), Arbitrary (f c), Arbitrary (a -> b), Arbitrary (b -> c), Eq (f c)) => f a -> f b -> f c -> QC Unit
```

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`


## Module Test.QuickCheck.Laws.Lattice

#### `checkLattice`

``` purescript
checkLattice :: forall m a. (Arbitrary a, Lattice a) => a -> QC Unit
```

- Associativity:
  - `a || (b || c) = (a || b) || c`
  - `a && (b && c) = (a && b) && c`
- Commutativity:
  - `a || b = b || a`
  - `a && b = b && a`
- Absorption:
  - `a || (a && b) = a`
  - `a && (a || b) = a`
- Idempotent:
  - `a || a = a`
  - `a && a = a`


## Module Test.QuickCheck.Laws.ModuloSemiring

#### `checkModuloSemiring`

``` purescript
checkModuloSemiring :: forall a. (ModuloSemiring a, Arbitrary a, Eq a) => a -> QC Unit
```

- Remainder: ```a / b * b + (a `mod` b) = a```


## Module Test.QuickCheck.Laws.Monad

#### `checkMonad`

``` purescript
checkMonad :: forall m a. (Monad m, Arbitrary a, Arbitrary (m a), CoArbitrary a, Eq (m a)) => m a -> QC Unit
```

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`


## Module Test.QuickCheck.Laws.Num

#### `checkNum`

``` purescript
checkNum :: forall a. (Num a, Arbitrary a, Eq a) => a -> QC Unit
```

- Commutative multiplication: `a * b = b * a`


## Module Test.QuickCheck.Laws.Ord

#### `checkOrd`

``` purescript
checkOrd :: forall m a. (Arbitrary a, Ord a) => a -> QC Unit
```

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`


## Module Test.QuickCheck.Laws.Ring

#### `checkRing`

``` purescript
checkRing :: forall a. (Ring a, Arbitrary a, Eq a) => a -> QC Unit
```

- Additive inverse: `a + (-a) = (-a) + a = zero`


## Module Test.QuickCheck.Laws.Semigroup

#### `checkSemigroup`

``` purescript
checkSemigroup :: forall s. (Semigroup s, Arbitrary s, Eq s) => s -> QC Unit
```

- Associativity: `(x <> y) <> z = x <> (y <> z)`


## Module Test.QuickCheck.Laws.Semigroupoid

#### `checkSemigroupoid`

``` purescript
checkSemigroupoid :: forall a b c d e. (Semigroupoid a, Arbitrary (a b c), Arbitrary (a c d), Arbitrary (a d e), Eq (a b e)) => a b c -> a d e -> QC Unit
```

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`


## Module Test.QuickCheck.Laws.Semiring

#### `checkSemiring`

``` purescript
checkSemiring :: forall a. (Semiring a, Arbitrary a, Eq a) => a -> QC Unit
```

- Commutative monoid under addition:
  - Associativity: `(a + b) + c = a + (b + c)`
  - Identity: `zero + a = a + zero = a`
  - Commutative: `a + b = b + a`
- Monoid under multiplication:
  - Associativity: `(a * b) * c = a * (b * c)`
  - Identity: `one * a = a * one = a`
- Multiplication distributes over addition:
  - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
  - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
- Annihiliation: `zero * a = a * zero = zero`



