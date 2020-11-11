module CST.Simple.ModuleBuilder
       ( addValue
       , addForeignJsValue
       , addType
       , addDataDecl
       , addNewtypeDecl
       , addClassDecl
       , addInstanceDecl
       , addInstanceChainDecl
       , addDeriveDecl
       , addDeriveNewtypeDecl
       ) where

import Prelude

import CST.Simple.Internal.Binder (Binder)
import CST.Simple.Internal.Declaration (DataCtor, Declaration, Fixity, FixityOp, Instance, InstanceBinding, declClass, declData, declDerive, declDeriveNewtype, declForeignValue, declInfix, declInstance, declInstanceChain, declNewtype, declSignature, declType, declValue, runDeclaration)
import CST.Simple.Internal.Expression (Expr, Guarded, grd_)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilderT, addCSTDeclaration, addForeignBinding)
import CST.Simple.Internal.Type (Constraint, Type)
import CST.Simple.Internal.TypeVarBinding (TypeVarBinding)
import Data.Tuple.Nested (type (/\))

-- Declarations

addValue ::
  forall m.
  Monad m =>
  { name :: String
  , type_ :: Type
  , binders :: Array Binder
  , expr :: Expr
  } ->
  ModuleBuilderT m Unit
addValue { name, type_, binders, expr } = do
  addDeclaration $ declSignature name type_
  addDeclaration $ declValue name binders (grd_ expr)

addForeignJsValue ::
  forall m.
  Monad m =>
  { name :: String
  , type_ :: Type
  , jsExpr :: String
  } ->
  ModuleBuilderT m Unit
addForeignJsValue { name, type_, jsExpr } = do
  addDeclaration $ declForeignValue name type_
  addForeignBinding $ "exports." <> name <> " = " <> jsExpr <> ";\n"


addType :: forall m. Monad m => String -> Array TypeVarBinding -> Type -> ModuleBuilderT m Unit
addType name fields type_' =
  addDeclaration $ declType name fields type_'

addDataDecl :: forall m. Monad m => String -> Array TypeVarBinding -> Array DataCtor -> ModuleBuilderT m Unit
addDataDecl name fields constructors' =
  addDeclaration $ declData name fields constructors'

addNewtypeDecl :: forall m. Monad m => String -> Array TypeVarBinding -> String -> Type -> ModuleBuilderT m Unit
addNewtypeDecl dataHdName fields name' type_' =
  addDeclaration $ declNewtype dataHdName fields name' type_'

addClassDecl :: forall m. Monad m => String -> Array TypeVarBinding -> Array Constraint -> Array (Array String /\ Array String) -> Array (String /\ Type) -> ModuleBuilderT m Unit
addClassDecl name fields super fundeps methods' =
  addDeclaration $ declClass name fields super fundeps methods'

addInstanceDecl :: forall m. Monad m => String -> Array Constraint -> String -> Array Type -> Array InstanceBinding -> ModuleBuilderT m Unit
addInstanceDecl name' constraints' class' types' body' =
  addDeclaration $ declInstance name' constraints' class' types' body'

addInstanceChainDecl :: forall m. Monad m => Instance -> Array Instance -> ModuleBuilderT m Unit
addInstanceChainDecl i1 is =
  addDeclaration $ declInstanceChain i1 is

addDeriveDecl :: forall m. Monad m => String -> Array Constraint -> String -> Array Type -> ModuleBuilderT m Unit
addDeriveDecl name constraints class' types =
  addDeclaration $ declDerive name constraints class' types

addDeriveNewtypeDecl :: forall m. Monad m => String -> Array Constraint -> String -> Array Type -> ModuleBuilderT m Unit
addDeriveNewtypeDecl name constraints class' types =
  addDeclaration $ declDeriveNewtype name constraints class' types

addSignatureDecl :: forall m. Monad m => String -> Type -> ModuleBuilderT m Unit
addSignatureDecl ident' type_' =
  addDeclaration $ declSignature ident' type_'

addValueDecl :: forall m. Monad m => String -> Array Binder -> Guarded -> ModuleBuilderT m Unit
addValueDecl name binders guarded =
  addDeclaration $ declValue name binders guarded

addInfixDecl :: forall m. Monad m => Fixity -> Int -> FixityOp -> ModuleBuilderT m Unit
addInfixDecl keyword precedence operator' =
  addDeclaration $ declInfix keyword precedence operator'

-- Common

addDeclaration :: forall m. Monad m => Declaration -> ModuleBuilderT m Unit
addDeclaration decl =
  addCSTDeclaration =<< runDeclaration decl
