{ name = "cst-simple"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "node-fs-aff"
  , "ps-cst"
  , "psci-support"
  , "spec"
  , "string-parsers"
  , "typelevel-prelude"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
