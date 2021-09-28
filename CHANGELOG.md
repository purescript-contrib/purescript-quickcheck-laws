# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:
- Provide `Arbitrary`-less law checks (#36 by @matthewleon, #57 by @JordanMartinez)

Bugfixes:

Other improvements:
- Fix integer overflow error in test for Ints (#58 by @JordanMartinez)

## [v6.0.1](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v6.0.1) - 2021-05-06

Other improvements:
- Added test for `Semiring`'s Annihilation law (#56 by @JordanMartinez)
- Migrated the library to the `contrib` organization (#54 by @thomashoneyman)
- Fixed warnings revealed by v0.14.1 PS release (#56 by @JordanMartinez)
- Installed transitive dependencies used in source code (#56 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v6.0.0) - 2021-02-26

- Added support for PureScript 0.14 and drop support for previous compiler versions.

## [v5.1.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v5.1.0) - 2020-01-29

- Added checks for `FunctorWithIndex` (@thought2)

## [v5.0.1](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v5.0.1) - 2019-05-30

- Some fixes for compatibility with PureScript 0.13 (@justinwoo)

## [v5.0.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v5.0.0) - 2019-04-17

- Updated `purescript-quickcheck` dependency to v6 (@paluh)

## [v4.0.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v4.0.0) - 2018-06-12

- Updated for PureScript 0.12
- The `Field` laws have been removed, as `DivisionRing` + `EuclideanRing` cover them

## [v3.0.1](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v3.0.1) - 2017-12-11

- Add missing newtype-derived `Enum` instances for the `A`-`D` test types

## [v3.0.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v3.0.0) - 2017-04-03

- Updated for PureScript 0.11

## [v2.1.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v2.1.0) - 2016-11-10

- Added property tests for `Foldable` (@jacereda)

## [v2.0.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v2.0.0) - 2016-10-23

- Updated dependencies for PureScript 0.10

## [v1.0.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v1.0.0) - 2016-06-09

- Updated for 1.0 core libraries.

## [v0.1.1](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v0.1.1) - 2016-02-01

- Opened the row used for effects for each of the checks (@rgrempel)

## [v0.1.0](https://github.com/purescript-contrib/purescript-quickcheck-laws/releases/tag/v0.1.0) - 2015-11-26

Initial versioned release
