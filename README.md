# Module Documentation

## Module Test.QuickCheck.Classes.Applicative

#### `checkApplicative`

``` purescript
checkApplicative :: forall f a b c. (Applicative f, Arbitrary a, Arbitrary b, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), CoArbitrary a, Eq (f a), Eq (f b), Eq (f c)) => f a -> f b -> f c -> QC Unit
```



## Module Test.QuickCheck.Laws.Apply

#### `checkApply`

``` purescript
checkApply :: forall f a b c. (Apply f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Eq (f c)) => f a -> f b -> f c -> QC Unit
```



## Module Test.QuickCheck.Laws.Bind

#### `checkBind`

``` purescript
checkBind :: forall m a. (Bind m, Arbitrary a, Arbitrary (m a), CoArbitrary a, Eq (m a)) => m a -> QC Unit
```



## Module Test.QuickCheck.Laws.Bounded

#### `checkBounded`

``` purescript
checkBounded :: forall m a. (Arbitrary a, Bounded a) => a -> QC Unit
```



## Module Test.QuickCheck.Laws.BoundedLattice

#### `checkBoundedLattice`

``` purescript
checkBoundedLattice :: forall m a. (Arbitrary a, BoundedLattice a) => a -> QC Unit
```



## Module Test.QuickCheck.Laws.Category

#### `checkCategory`

``` purescript
checkCategory :: forall a b. (Category a, Arbitrary (a b b), Eq (a b b)) => a b b -> QC Unit
```



## Module Test.QuickCheck.Laws.ComplementedLattice

#### `checkComplementedLattice`

``` purescript
checkComplementedLattice :: forall m a. (Arbitrary a, ComplementedLattice a) => a -> QC Unit
```



## Module Test.QuickCheck.Laws.DistributiveLattice

#### `checkDistributiveLattice`

``` purescript
checkDistributiveLattice :: forall m a. (Arbitrary a, DistributiveLattice a) => a -> QC Unit
```



## Module Test.QuickCheck.Laws.Eq

#### `checkEq`

``` purescript
checkEq :: forall m a. (Arbitrary a, Eq a) => a -> QC Unit
```



## Module Test.QuickCheck.Laws.Functor

#### `checkFunctor`

``` purescript
checkFunctor :: forall f a b c. (Functor f, Arbitrary a, Arbitrary (f a), Arbitrary (f c), Arbitrary (a -> b), Arbitrary (b -> c), Eq (f c)) => f a -> f b -> f c -> QC Unit
```



## Module Test.QuickCheck.Laws.Lattice

#### `checkLattice`

``` purescript
checkLattice :: forall m a. (Arbitrary a, Lattice a) => a -> QC Unit
```



## Module Test.QuickCheck.Laws.Monad

#### `checkMonad`

``` purescript
checkMonad :: forall m a. (Monad m, Arbitrary a, Arbitrary (m a), CoArbitrary a, Eq (m a)) => m a -> QC Unit
```



## Module Test.QuickCheck.Laws.Ord

#### `checkOrd`

``` purescript
checkOrd :: forall m a. (Arbitrary a, Ord a) => a -> QC Unit
```



## Module Test.QuickCheck.Laws.Semigroup

#### `checkSemigroup`

``` purescript
checkSemigroup :: forall s. (Semigroup s, Arbitrary s, Eq s) => s -> QC Unit
```



## Module Test.QuickCheck.Laws.Semigroupoid

#### `checkSemigroupoid`

``` purescript
checkSemigroupoid :: forall a b c d e. (Semigroupoid a, Arbitrary (a b c), Arbitrary (a c d), Arbitrary (a d e), Eq (a b e)) => a b c -> a d e -> QC Unit
```




