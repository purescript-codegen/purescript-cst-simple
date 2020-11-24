module CST.Simple.ModuleBuilder
       ( addKind
       , addType
       , addNewtype
       , addValue
       , addForeignJsValue
       , addForeignData
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
import CST.Simple.Internal.Declaration (DataCtor, Declaration, Fixity, FixityOp, Instance, InstanceBinding, declClass, declData, declDerive, declDeriveNewtype, declForeignData, declForeignKind, declForeignValue, declInfix, declInstance, declInstanceChain, declNewtype, declSignature, declType, declValue, runDeclaration)
import CST.Simple.Internal.Export (Export, exportKind, exportType, exportValue, runExport)
import CST.Simple.Internal.Expression (Expr, Guarded, grd_)
import CST.Simple.Internal.Kind (Kind)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, addCSTDeclaration, addCSTExport, addForeignBinding)
import CST.Simple.Internal.Type (Constraint, Type)
import CST.Simple.Internal.TypeVarBinding (TypeVarBinding)
import CST.Simple.Types (DataExport(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))

-- Declarations

addKind ::
  { name :: String
  , export :: Boolean
  } ->
  ModuleBuilder Unit
addKind { name, export } = do
  addDeclaration $ declForeignKind name
  when export $ addExport $ exportKind name

addType ::
  { name :: String
  , typeVarBindings :: Array TypeVarBinding
  , type_ :: Type
  , export :: Boolean
  } ->
  ModuleBuilder Unit
addType { name, typeVarBindings, type_, export } = do
  addDeclaration $ declType name typeVarBindings type_
  when export $ addExport $ exportType name DataExportType

addNewtype ::
  { name :: String
  , export :: Maybe DataExport
  , typeVarBindings :: Array TypeVarBinding
  , consName :: String
  , type_ :: Type
  } ->
  ModuleBuilder Unit
addNewtype { name, export, typeVarBindings, consName, type_ } = do
  addDeclaration $ declNewtype name typeVarBindings consName type_
  traverse_ (addExport <<< exportType name) export

addValue ::
  { name :: String
  , type_ :: Type
  , binders :: Array Binder
  , expr :: Expr
  , export :: Boolean
  } ->
  ModuleBuilder Unit
addValue { name, type_, binders, expr, export } = do
  addDeclaration $ declSignature name type_
  addDeclaration $ declValue name binders (grd_ expr)
  when export $ addExport $ exportValue name

addForeignJsValue ::
  { name :: String
  , type_ :: Type
  , jsExpr :: String
  , export :: Boolean
  } ->
  ModuleBuilder Unit
addForeignJsValue { name, type_, jsExpr, export } = do
  addDeclaration $ declForeignValue name type_
  addForeignBinding $ "exports." <> name <> " = " <> jsExpr <> ";\n"
  when export $ addExport $ exportValue name

addForeignData ::
  { name :: String
  , kind_ :: Kind
  , export :: Boolean
  } ->
  ModuleBuilder Unit
addForeignData { name, kind_, export } = do
  addDeclaration $ declForeignData name kind_
  when export $ addExport $ exportType name DataExportType

addDataDecl :: String -> Array TypeVarBinding -> Array DataCtor -> ModuleBuilder Unit
addDataDecl name fields constructors' =
  addDeclaration $ declData name fields constructors'

addNewtypeDecl :: String -> Array TypeVarBinding -> String -> Type -> ModuleBuilder Unit
addNewtypeDecl dataHdName fields name' type_' =
  addDeclaration $ declNewtype dataHdName fields name' type_'

addClassDecl :: String -> Array TypeVarBinding -> Array Constraint -> Array (Array String /\ Array String) -> Array (String /\ Type) -> ModuleBuilder Unit
addClassDecl name fields super fundeps methods' =
  addDeclaration $ declClass name fields super fundeps methods'

addInstanceDecl :: String -> Array Constraint -> String -> Array Type -> Array InstanceBinding -> ModuleBuilder Unit
addInstanceDecl name' constraints' class' types' body' =
  addDeclaration $ declInstance name' constraints' class' types' body'

addInstanceChainDecl :: Instance -> Array Instance -> ModuleBuilder Unit
addInstanceChainDecl i1 is =
  addDeclaration $ declInstanceChain i1 is

addDeriveDecl :: String -> Array Constraint -> String -> Array Type -> ModuleBuilder Unit
addDeriveDecl name constraints class' types =
  addDeclaration $ declDerive name constraints class' types

addDeriveNewtypeDecl :: String -> Array Constraint -> String -> Array Type -> ModuleBuilder Unit
addDeriveNewtypeDecl name constraints class' types =
  addDeclaration $ declDeriveNewtype name constraints class' types

addSignatureDecl :: String -> Type -> ModuleBuilder Unit
addSignatureDecl ident' type_' =
  addDeclaration $ declSignature ident' type_'

addValueDecl :: String -> Array Binder -> Guarded -> ModuleBuilder Unit
addValueDecl name binders guarded =
  addDeclaration $ declValue name binders guarded

addInfixDecl :: Fixity -> Int -> FixityOp -> ModuleBuilder Unit
addInfixDecl keyword precedence operator' =
  addDeclaration $ declInfix keyword precedence operator'

-- Common

addDeclaration :: Declaration -> ModuleBuilder Unit
addDeclaration decl =
  addCSTDeclaration =<< runDeclaration decl

addExport :: Export -> ModuleBuilder Unit
addExport decl =
  addCSTExport =<< runExport decl
