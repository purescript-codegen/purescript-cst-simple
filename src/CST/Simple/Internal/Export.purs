module CST.Simple.Internal.Export
       ( Export
       , runExport
       , exportValue
       , exportOp
       , exportType
       , exportTypeSomeMembers
       , exportClass
       , exportKind
       , exportModule
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder)
import CST.Simple.Internal.Utils (exceptM)
import CST.Simple.Names (readName)
import CST.Simple.Types (DataExport(..))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Language.PS.CST as CST

newtype Export = Export (Either CodegenError CST.Export)

derive newtype instance exportEq :: Eq Export
instance exportShow :: Show Export where
  show (Export e) = "(Export " <> show e <> ")"

runExport :: Export -> ModuleBuilder CST.Export
runExport (Export e) = exceptM e

exportValue :: String -> Export
exportValue ident = Export $ CST.ExportValue <$> readName ident

exportOp :: String -> Export
exportOp op = Export $ CST.ExportOp <$> readName op

exportType :: String -> DataExport -> Export
exportType typ de =
  Export $ CST.ExportType <$> readName typ <*> pure dataMembers

  where
    dataMembers = case de of
      DataExportType -> Nothing
      DataExportAll -> Just CST.DataAll

exportTypeSomeMembers :: String -> Array String -> Export
exportTypeSomeMembers typ members = Export ado
  typ' <- readName typ
  members' <- traverse readName members
  in CST.ExportType typ' (Just $ CST.DataEnumerated members')

exportClass :: String -> Export
exportClass class_ = Export $ CST.ExportClass <$> readName class_

exportKind :: String -> Export
exportKind kind_ = Export $ CST.ExportKind <$> readName kind_

exportModule :: String -> Export
exportModule moduleName = Export $ CST.ExportModule <$> readName moduleName
