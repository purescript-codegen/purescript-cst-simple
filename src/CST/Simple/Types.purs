module CST.Simple.Types
       ( ModuleEntry
       , Project
       , ProjectSettings
       , DataExport(..)
       ) where

import Data.Maybe (Maybe)
import Dodo (PrintOptions)
import Language.PS.CST as CST
import Node.Path (FilePath)

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
  , rmDirectoryFilesPreRun :: Boolean
  }

data DataExport =
  DataExportType
  | DataExportAll
