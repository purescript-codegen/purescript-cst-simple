{ name = "cst-simple"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "node-fs-aff"
  , "parsing"
  , "ps-cst"
  , "psci-support"
  , "spec"
  , "typelevel-prelude"
  ]  
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
