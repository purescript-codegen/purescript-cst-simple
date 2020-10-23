let additions =
  { dodo-printer =
    { dependencies =
      [ "aff"
      , "ansi"
      , "avar"
      , "console"
      , "effect"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "minibench"
      , "node-child-process"
      , "node-fs-aff"
      , "node-process"
      , "psci-support"
      , "strings"
      ]
    , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
    , version = "v1.0.8"
    }
  , ps-cst =
    { dependencies =
      [ "ansi"
      , "console"
      , "dodo-printer"
      , "effect"
      , "generics-rep"
      , "node-fs-aff"
      , "node-path"
      , "psci-support"
      , "record"
      , "spec"
      , "strings"
      ]
    , repo = "https://github.com/jvliwanag/purescript-ps-cst.git"
    , version = "776749a"
    }
  }

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201007/packages.dhall sha256:35633f6f591b94d216392c9e0500207bb1fec42dd355f4fecdfd186956567b6b

in  upstream // additions
