{ name = "quickcheck-laws"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "quickcheck"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
