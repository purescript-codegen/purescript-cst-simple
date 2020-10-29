module CST.Simple.Internal.Import
       ( class AsImport
       , asImport
       ) where

import CST.Simple.Names (Ident, TypedConstructorName(..))
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST

class AsImport a where
  asImport :: a -> CST.Import

instance asImportIdent :: AsImport Ident where
  asImport = CST.ImportValue

instance asImportOp :: AsImport (CST.OpName CST.OpNameType_ValueOpName) where
  asImport = CST.ImportOp

instance asImportType :: AsImport (CST.ProperName CST.ProperNameType_TypeName) where
  asImport n = CST.ImportType n Nothing

instance asImportTypeOp :: AsImport (CST.OpName CST.OpNameType_TypeOpName) where
  asImport = CST.ImportTypeOp

instance asImportClass :: AsImport (CST.ProperName CST.ProperNameType_ClassName) where
  asImport = CST.ImportClass

instance asImportKind :: AsImport (CST.ProperName CST.ProperNameType_KindName) where
  asImport = CST.ImportKind

instance asImportTypedConstructorName :: AsImport TypedConstructorName where
  asImport (TypedConstructorName t _) =
    CST.ImportType t (Just CST.DataAll)
