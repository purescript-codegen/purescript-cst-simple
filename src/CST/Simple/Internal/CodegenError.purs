module CST.Simple.Internal.CodegenError
       ( CodegenError(..)
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

data CodegenError =
  InvalidModuleName String
  | DuplicateModuleName String
  | InvalidDataHeadName String CodegenError
  | InvalidTypeName String
  | InvalidConstructorName String
  | InvalidClassName String
  | InvalidKindName String
  | InvalidIdent String
  | InvalidTypeOpName String
  | InvalidQualifiedModule String String
  | InvalidQualifiedName String String (Maybe CodegenError)
  | DuplicateDeclName String

derive instance codegenErrorEq :: Eq CodegenError
derive instance codegenErrorGeneric :: Generic CodegenError _
instance codegenErrorShow :: Show CodegenError where
  show x = genericShow x
