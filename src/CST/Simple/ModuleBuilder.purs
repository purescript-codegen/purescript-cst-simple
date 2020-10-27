module CST.Simple.ModuleBuilder
       ( addTypeDecl
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilderT, addDeclaration, mkPName)
import CST.Simple.Internal.Type (class AsTyp, runTyp')
import CST.Simple.Names (pnameToProperName)
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST

-- Declarations

addTypeDecl :: forall m t. Monad m => AsTyp t => String -> t -> ModuleBuilderT m Unit
addTypeDecl name t = do
  pname <- mkPName name
  type_ <- runTyp' t
  addDeclaration pname $
    CST.DeclType
    { comments: Nothing
    , head: CST.DataHead
      { dataHdName: pnameToProperName pname
      , dataHdVars: []
      }
    , type_
    }
