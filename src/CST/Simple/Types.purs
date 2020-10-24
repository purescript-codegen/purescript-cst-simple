module CST.Simple.Types
       ( CodegenError(..)
       , ModuleContent
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Language.PS.CST as CST

data CodegenError =
  InvalidModuleName String
  | DuplicateModuleName String
  | InvalidProperName String
  | InvalidIdent String
  | InvalidQualifiedName String
  | DuplicateDeclName String

derive instance codegenErrorEq :: Eq CodegenError
derive instance codegenErrorGeneric :: Generic CodegenError _
instance codegenErrorShow :: Show CodegenError where
  show = genericShow

type ModuleContent =
  { imports :: Array CST.ImportDecl
  , exports :: Array CST.Export
  , declarations :: Array CST.Declaration
  }
