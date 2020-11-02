module CST.Simple.ModuleBuilder
       ( addTypeDecl
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilderT, addDeclaration, mkName)
import CST.Simple.Internal.Type (Type, runType)
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST

-- Declarations

addTypeDecl :: forall m. Monad m => String -> Type -> ModuleBuilderT m Unit
addTypeDecl name t = do
  head <- mkName name
  type_ <- runType t
  addDeclaration name $
    CST.DeclType
    { comments: Nothing
    , head
    , type_
    }
