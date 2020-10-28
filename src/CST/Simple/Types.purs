module CST.Simple.Types
       ( ModuleContent
       ) where

import Language.PS.CST as CST

type ModuleContent =
  { imports :: Array CST.ImportDecl
  , exports :: Array CST.Export
  , declarations :: Array CST.Declaration
  }
