module CST.Simple.Internal.Import
       ( class AsImport
       , asImport
       ) where

import CST.Simple.Names (Ident, OpName, ProperName)
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST

class AsImport a where
  asImport :: a -> CST.Import

instance asImportIdent :: AsImport Ident where
  asImport = CST.ImportValue

instance asImportOp :: AsImport (OpName CST.OpNameType_ValueOpName) where
  asImport = CST.ImportOp

instance asImportType :: AsImport (ProperName CST.ProperNameType_TypeName) where
  asImport n = CST.ImportType n Nothing

instance asImportTypeOp :: AsImport (OpName CST.OpNameType_TypeOpName) where
  asImport = CST.ImportTypeOp

instance asImportClass :: AsImport (ProperName CST.ProperNameType_ClassName) where
  asImport = CST.ImportClass

instance asImportKind :: AsImport (ProperName CST.ProperNameType_KindName) where
  asImport = CST.ImportKind
