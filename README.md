# Module Documentation

## Module Test.QuickCheck.Laws.Control.Alt

#### `checkAlt`

``` purescript
checkAlt :: forall f a b. (Alt f, Arbitrary a, Arbitrary (a -> b), Arbitrary (f a), Eq (f a), Eq (f b)) => Proxy2 f -> Proxy a -> Proxy b -> QC Unit
```

- Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
- Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`


## Module Test.QuickCheck.Laws.Control.Alternative

#### `checkAlternative`

``` purescript
checkAlternative :: forall f a b. (Alternative f, Arbitrary a, Arbitrary (f (a -> b)), Arbitrary (f a), Eq (f a), Eq (f b)) => Proxy2 f -> Proxy a -> Proxy b -> QC Unit
```

- Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
- Annihilation: `empty <*> x = empty`


## Module Test.QuickCheck.Laws.Control.Applicative

#### `checkApplicative`

``` purescript
checkApplicative :: forall f a b c. (Applicative f, Arbitrary a, Arbitrary b, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Coarbitrary a, Eq (f a), Eq (f b), Eq (f c)) => Proxy2 f -> Proxy a -> Proxy b -> Proxy c -> QC Unit
```

- Identity: `(pure id) <*> v = v`
- Composition: `(pure (<<<)) <*> f <*> g <*> h = f <*> (g <*> h)`
- Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
- Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`


## Module Test.QuickCheck.Laws.Control.Apply

#### `checkApply`

``` purescript
checkApply :: forall f a b c. (Apply f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Eq (f c)) => Proxy2 f -> Proxy a -> Proxy b -> Proxy c -> QC Unit
```

- Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`


## Module Test.QuickCheck.Laws.Control.Bind

#### `checkBind`

``` purescript
checkBind :: forall m a. (Bind m, Arbitrary a, Arbitrary (m a), Coarbitrary a, Eq (m a)) => Proxy2 m -> Proxy a -> QC Unit
```

- Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`


## Module Test.QuickCheck.Laws.Control.Category

#### `checkCategory`

``` purescript
checkCategory :: forall a b c. (Category a, Arbitrary (a b c), Eq (a b c)) => Proxy3 a -> Proxy b -> Proxy c -> QC Unit
```

- Identity: `id <<< p = p <<< id = p`


## Module Test.QuickCheck.Laws.Control.Comonad

#### `checkComonad`

``` purescript
checkComonad :: forall w a b. (Comonad w, Arbitrary a, Arbitrary (w a), Arbitrary (w a -> b), Eq b, Eq (w a)) => Proxy2 w -> Proxy a -> Proxy b -> QC Unit
```

- Left Identity: `extract <<= x = x`
- Right Identity: `extract (f <<= x) = f x`


## Module Test.QuickCheck.Laws.Control.Extend

#### `checkExtend`

``` purescript
checkExtend :: forall w a b c. (Extend w, Arbitrary (w a), Arbitrary (w a -> b), Arbitrary (w b -> c), Eq (w c)) => Proxy2 w -> Proxy a -> Proxy b -> Proxy c -> QC Unit
```

- Associativity: `extend f <<< extend g = extend (f <<< extend g)`


## Module Test.QuickCheck.Laws.Control.Monad

#### `checkMonad`

``` purescript
checkMonad :: forall m a. (Monad m, Arbitrary a, Arbitrary (m a), Coarbitrary a, Eq (m a)) => Proxy2 m -> Proxy a -> QC Unit
```

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`


## Module Test.QuickCheck.Laws.Control.MonadPlus

#### `checkMonadPlus`

``` purescript
checkMonadPlus :: forall m a b. (MonadPlus m, Arbitrary (a -> m b), Arbitrary (m a), Eq (m b)) => Proxy2 m -> Proxy a -> Proxy b -> QC Unit
```

- Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
- Annihilation: `empty >>= f = empty`


## Module Test.QuickCheck.Laws.Control.Plus

#### `checkPlus`

``` purescript
checkPlus :: forall f a b. (Plus f, Arbitrary a, Arbitrary (a -> b), Arbitrary (f a), Eq (f a), Eq (f b)) => Proxy2 f -> Proxy a -> Proxy b -> QC Unit
```

- Left identity: `empty <|> x == x`
- Right identity: `x <|> empty == x`
- Annihilation: `f <$> empty == empty`


## Module Test.QuickCheck.Laws.Control.Semigroupoid

#### `checkSemigroupoid`

``` purescript
checkSemigroupoid :: forall a b c d e. (Semigroupoid a, Arbitrary (a b c), Arbitrary (a c d), Arbitrary (a d e), Eq (a b e)) => Proxy3 a -> Proxy b -> Proxy c -> Proxy d -> Proxy e -> QC Unit
```

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`


## Module Test.QuickCheck.Laws.Data.Bounded

#### `checkBounded`

``` purescript
checkBounded :: forall a. (Arbitrary a, Bounded a) => Proxy a -> QC Unit
```

- Ordering: `bottom <= a <= top`


## Module Test.QuickCheck.Laws.Data.BoundedLattice

#### `checkBoundedLattice`

``` purescript
checkBoundedLattice :: forall a. (Arbitrary a, BoundedLattice a) => Proxy a -> QC Unit
```

- Identity:
  - `a || bottom = a`
  - `a && top = a`
- Annihiliation:
  - `a || top = top`
  - `a && bottom = bottom`


## Module Test.QuickCheck.Laws.Data.ComplementedLattice

#### `checkComplementedLattice`

``` purescript
checkComplementedLattice :: forall a. (Arbitrary a, ComplementedLattice a) => Proxy a -> QC Unit
```

- Complemented:
  - `not a || a == top`
  - `not a && a == bottom`
- Double negation:
  - `not <<< not == id`


## Module Test.QuickCheck.Laws.Data.DistributiveLattice

#### `checkDistributiveLattice`

``` purescript
checkDistributiveLattice :: forall a. (Arbitrary a, DistributiveLattice a) => Proxy a -> QC Unit
```

- Distributivity: `x && (y || z) = (x && y) || (x && z)`


## Module Test.QuickCheck.Laws.Data.DivisionRing

#### `checkDivisionRing`

``` purescript
checkDivisionRing :: forall a. (DivisionRing a, Arbitrary a, Eq a) => Proxy a -> QC Unit
```

- Multiplicative inverse: `(one / x) * x = one`


## Module Test.QuickCheck.Laws.Data.Eq

#### `checkEq`

``` purescript
checkEq :: forall a. (Arbitrary a, Eq a) => Proxy a -> QC Unit
```

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`
- Negation: `x /= y = not (x == y)`

#### `cast`

``` purescript
cast :: forall a b. a -> b
```


#### `debug`

``` purescript
debug :: forall a b. a -> b -> b
```



## Module Test.QuickCheck.Laws.Data.Functor

#### `checkFunctor`

``` purescript
checkFunctor :: forall f a b. (Functor f, Arbitrary (f a), Arbitrary (a -> b), Arbitrary (b -> a), Eq (f a)) => Proxy2 f -> Proxy a -> Proxy b -> QC Unit
```

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`


## Module Test.QuickCheck.Laws.Data.Lattice

#### `checkLattice`

``` purescript
checkLattice :: forall a. (Arbitrary a, Lattice a) => Proxy a -> QC Unit
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


## Module Test.QuickCheck.Laws.Data.ModuloSemiring

#### `checkModuloSemiring`

``` purescript
checkModuloSemiring :: forall a. (ModuloSemiring a, Arbitrary a, Eq a) => Proxy a -> QC Unit
```

- Remainder: ```a / b * b + (a `mod` b) = a```


## Module Test.QuickCheck.Laws.Data.Monoid

#### `checkMonoid`

``` purescript
checkMonoid :: forall m. (Monoid m, Arbitrary m, Eq m) => Proxy m -> QC Unit
```

- Left identity: `mempty <> x = x`
- Right identity: `x <> mempty = x`


## Module Test.QuickCheck.Laws.Data.Num

#### `checkNum`

``` purescript
checkNum :: forall a. (Num a, Arbitrary a, Eq a) => Proxy a -> QC Unit
```

- Commutative multiplication: `a * b = b * a`


## Module Test.QuickCheck.Laws.Data.Ord

#### `checkOrd`

``` purescript
checkOrd :: forall a. (Arbitrary a, Ord a) => Proxy a -> QC Unit
```

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`


## Module Test.QuickCheck.Laws.Data.Ring

#### `checkRing`

``` purescript
checkRing :: forall a. (Ring a, Arbitrary a, Eq a) => Proxy a -> QC Unit
```

- Additive inverse: `a + (-a) = (-a) + a = zero`


## Module Test.QuickCheck.Laws.Data.Semigroup

#### `checkSemigroup`

``` purescript
checkSemigroup :: forall s. (Semigroup s, Arbitrary s, Eq s) => Proxy s -> QC Unit
```

- Associativity: `(x <> y) <> z = x <> (y <> z)`


## Module Test.QuickCheck.Laws.Data.Semiring

#### `checkSemiring`

``` purescript
checkSemiring :: forall a. (Semiring a, Arbitrary a, Eq a) => Proxy a -> QC Unit
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



