module CST.Simple.ModuleBuilder
       ( addTypeDecl
       ) where

import Prelude

import CST.Simple.Internal.Declaration (Declaration, declType, runDeclaration)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilderT, addCSTDeclaration)
import CST.Simple.Internal.Type (Type)
import CST.Simple.Internal.TypeVarBinding (TypeVarBinding)

-- Declarations

addTypeDecl :: forall m. Monad m => String -> Array TypeVarBinding -> Type -> ModuleBuilderT m Unit
addTypeDecl name fields type_ =
  addDeclaration $ declType name fields type_

addDeclaration :: forall m. Monad m => Declaration -> ModuleBuilderT m Unit
addDeclaration decl =
  addCSTDeclaration =<< runDeclaration decl
