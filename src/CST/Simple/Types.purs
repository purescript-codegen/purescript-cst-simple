module CST.Simple.Types
       ( ModuleContent
       , ModuleEntry
       , Project
       , ProjectSettings
       ) where

import Data.Maybe (Maybe)
import Dodo (PrintOptions)
import Language.PS.CST as CST
import Node.Path (FilePath)

type ModuleContent =
  { imports :: Array CST.ImportDecl
  , exports :: Array CST.Export
  , declarations :: Array CST.Declaration
  , foreignBinding :: Maybe String
  }

type ModuleEntry =
  { cstModule :: CST.Module
  , foreignBinding :: Maybe String
  }

type Project =
  { modules :: Array ModuleEntry
  }

type ProjectSettings =
  { outputDirectory :: FilePath
  , printOptions :: PrintOptions
  }
