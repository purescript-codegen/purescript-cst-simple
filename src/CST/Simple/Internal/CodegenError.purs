module CST.Simple.Internal.CodegenError
       ( CodegenError(..)
       ) where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

data CodegenError =
  InvalidModuleName String
  | MissingExports
  | InvalidTypeName String
  | InvalidConstructorName String
  | InvalidClassName String
  | InvalidKindName String
  | InvalidIdent String
  | InvalidTypeOpName String
  | InvalidValueOpName String
  | InvalidQualifiedModule String String
  | InvalidQualifiedName String String (Maybe CodegenError)
  | MissingCaseOfHeadBinders
  | MissingCaseOfBranches
  | MissingCaseOfBranchBinders
  | MissingDoStatements
  | MissingInstanceHeadTypes

  | MultiCodegenError (Array CodegenError)

derive instance codegenErrorEq :: Eq CodegenError
derive instance codegenErrorOrd :: Ord CodegenError
derive instance codegenErrorGeneric :: Generic CodegenError _
instance codegenErrorShow :: Show CodegenError where
  show x = genericShow x

instance codegenErrorSemigroup :: Semigroup CodegenError where
  append (MultiCodegenError esL) (MultiCodegenError esR) =
    MultiCodegenError (esL <> esR)
  append (MultiCodegenError esL) r =
    MultiCodegenError (Array.snoc esL r)
  append l (MultiCodegenError esR) =
    MultiCodegenError (Array.cons l esR)
  append l r =
    MultiCodegenError [ l, r ]
