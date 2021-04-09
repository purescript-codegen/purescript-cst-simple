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
        , version = "v2.0.0"
        }
      , ps-cst =
        { dependencies =
          [ "console"
          , "effect"
          , "psci-support"
          , "record"
          , "strings"
          , "spec"
          , "node-path"
          , "node-fs-aff"
          , "ansi"
          , "dodo-printer"
          ]
        , repo = "https://github.com/purescript-codegen/purescript-ps-cst.git"
        , version = "1339dd3"
        }
      }

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210406/packages.dhall sha256:7b6af643c2f61d936878f58b613fade6f3cb39f2b4a310f6095784c7b5285879

in  upstream // additions
