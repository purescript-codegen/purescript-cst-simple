module CST.Simple.Types
       ( CodegenError(..)
       , ModuleSpec(..)
       , getModuleName
       ) where

import Prelude

import CST.Simple.Names (ModuleName)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data CodegenError =
  InvalidModuleName String
  | DuplicateModuleName String

derive instance codegenErrorEq :: Eq CodegenError
derive instance codegenErrorGeneric :: Generic CodegenError _
instance codegenErrorShow :: Show CodegenError where
  show = genericShow

newtype ModuleSpec = ModuleSpec ModuleName

derive newtype instance moduleSpecEq :: Eq ModuleSpec
derive newtype instance moduleSpecShow :: Show ModuleSpec

getModuleName :: ModuleSpec -> ModuleName
getModuleName (ModuleSpec s) = s
