# QuickCheck Laws

[![CI](https://github.com/purescript-contrib/purescript-quickcheck-laws/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-quickcheck-laws/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-quickcheck-laws.svg)](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-quickcheck-laws/badge)](https://pursuit.purescript.org/packages/purescript-quickcheck-laws)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)

QuickCheck-powered law tests for PureScript's core typeclasses.

## Installation

Install `quickcheck-laws` with [Spago](https://github.com/purescript/spago):

```sh
spago install quickcheck-laws
```

## Quick start

Below is an example of how to test the laws of the `Functor` typeclass for a new
type `Pair` that we create. On running this code, 1000 tests of the two
`Functor` laws (identity and composition) will be sucessfully run.

```purs
module Main (main) where

-- The relevent imports
import Prelude
import Effect (Effect)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Type.Proxy (Proxy(Proxy))

-- A type to test the `Functor` instance of
data Pair a = Pair a a

-- We need an `Eq` instance to check whether two different calls to `map` result
-- in the same value
derive instance Eq a => Eq (Pair a)

-- We need to be able to generate values of type `Pair a`, assuming that we can
-- generate values of type `a`
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

-- The `Functor` that we want to test. There are multiple ways of defining a
-- functor over `Pair` but here we pick a simple and obvious one
instance Functor Pair where
  map f (Pair x y) = Pair (f x) (f y)

-- We are going to need to "pass `Pair` into the function `checkFunctor`". Since
-- we can't pass types (or type constructors) into functions directly, we create
-- a proxy that we can pass in
proxy :: Proxy Pair
proxy = Proxy

-- Finally, we can run the test by passing the proxy into `checkFunctor`
main :: Effect Unit
main = do
  checkFunctor proxy
```

The above code implements a law-abiding instance of `Functor` and therefore the
tests all pass with a message:
```
Checking 'Identity' law for Functor
1000/1000 test(s) passed.
Checking 'Composition' law for Functor
1000/1000 test(s) passed.
```

If we change the implementation of map to, say,
```purs
  map _ (Pair x y) = Pair y x
```
we get an instance of `Functor` that isn't at all law-abiding. It ignores the
first argument and swaps the elements of the `Pair`. Sure enough, if we run the
tests on this version we get an error message informing us that one of the tests
has failed.

Additional examples of successful tests can be found in
[the test suite](./test).

## Documentation

`quickcheck-laws` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-quickcheck-laws).
2. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-quickcheck-laws/issues) if you have encountered a bug or problem.
- [Search or start a thread on the PureScript Discourse](https://discourse.purescript.org) if you have general questions. You can also ask questions in the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)).

## Contributing

You can contribute to `quickcheck-laws` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-quickcheck-laws/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
