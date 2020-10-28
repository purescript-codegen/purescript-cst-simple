module CST.Simple.ModuleBuilder
       ( addTypeDecl
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilderT, addDeclaration, mkName)
import CST.Simple.Internal.Type (class AsTyp, runTyp')
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST

-- Declarations

addTypeDecl :: forall m t. Monad m => AsTyp t => String -> t -> ModuleBuilderT m Unit
addTypeDecl name t = do
  head <- mkName name
  type_ <- runTyp' t
  addDeclaration name $
    CST.DeclType
    { comments: Nothing
    , head
    , type_
    }
