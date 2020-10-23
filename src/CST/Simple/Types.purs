module CST.Simple.Types
       ( CodegenError(..)
       , ModuleContent
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data CodegenError =
  InvalidModuleName String
  | DuplicateModuleName String

derive instance codegenErrorEq :: Eq CodegenError
derive instance codegenErrorGeneric :: Generic CodegenError _
instance codegenErrorShow :: Show CodegenError where
  show = genericShow

type ModuleContent =
  {
  }
