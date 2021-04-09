module CST.Simple.Internal.CodegenError
       ( NameError
       , CodegenError(..)
       ) where

import Prelude

import CST.Simple.NameFormat (NameFormat)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type NameError =
  { given :: String
  , pos :: Int
  , msg :: String
  , allowQualified :: Boolean
  , allowUnqualified :: Boolean
  , allowAlias :: Boolean
  , nameFormat :: NameFormat
  }

data CodegenError =
  MissingExports
  | IllegalReExportOnExportAll
  | InvalidName NameError
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
