{ name = "quickcheck-laws"
, dependencies = [ "enums", "prelude", "quickcheck" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
