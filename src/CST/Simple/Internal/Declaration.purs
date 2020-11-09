module CST.Simple.Internal.Declaration
       ( Declaration
       , runDeclaration
       , declData
       , declType
       , declNewtype
       , declClass
       , declInstance
       , declInstanceChain
       , declDerive
       , declDeriveNewtype
       , DataCtor
       , dataCtor
       , runDataCtor
       , Instance
       , runInstance
       , instance_
       , InstanceBinding
       , runInstanceBinding
       , instanceBSig
       , instanceBName
       ) where

import Prelude

import CST.Simple.Internal.Binder (Binder, runBinder)
import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.Expr (Guarded, runGuarded)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualName)
import CST.Simple.Internal.Type (Constraint, Type, runConstraint, runType)
import CST.Simple.Internal.TypeVarBinding (TypeVarBinding, runTypeVarBinding)
import CST.Simple.Internal.Utils (requireNonEmptyArray)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Language.PS.CST as CST

newtype Declaration =
  Declaration (ModuleBuilder CST.Declaration)

runDeclaration :: forall m. Monad m => Declaration -> ModuleBuilderT m CST.Declaration
runDeclaration (Declaration mb) = liftModuleBuilder mb

declData :: String -> Array TypeVarBinding -> Array DataCtor -> Declaration
declData n vs cs = Declaration ado
  head <- mkDataHead n vs
  constructors <- traverse runDataCtor cs
  in CST.DeclData { comments: Nothing, head, constructors }

declType :: String -> Array TypeVarBinding -> Type -> Declaration
declType n vs t = Declaration ado
  head <- mkDataHead n vs
  type_ <- runType t
  in CST.DeclType { comments: Nothing, head, type_ }

declNewtype :: String -> Array TypeVarBinding -> String -> Type -> Declaration
declNewtype n vs c t = Declaration ado
  head <- mkDataHead n vs
  name <- mkName c
  type_ <- runType t
  in CST.DeclNewtype { comments: Nothing, head, name, type_ }

declClass :: String -> Array TypeVarBinding -> Array Constraint -> Array (Array String /\ Array String) -> Array (String /\ Type) -> Declaration
declClass n vs ss fds ms = Declaration ado
  head <- mkClassHead n vs ss fds
  methods <- traverse (uncurry mkMethod) ms
  in CST.DeclClass { comments: Nothing, head, methods }

  where
    mkMethod i t = ado
      ident <- mkName i
      type_ <- runType t
      in { ident, type_ }

declInstance :: String -> Array Constraint -> String -> Array Type -> Array InstanceBinding -> Declaration
declInstance name' constraints' class' types' body' =
  declInstanceChain (instance_ name' constraints' class' types' body') []

declInstanceChain :: Instance -> Array Instance -> Declaration
declInstanceChain i1 is = Declaration ado
  instances <- traverse runInstance (NonEmptyArray.cons' i1 is)
  in CST.DeclInstanceChain { comments: Nothing, instances }

declDerive :: String -> Array Constraint -> String -> Array Type -> Declaration
declDerive = declDerive' CST.DeclDeriveType_Odrinary

declDeriveNewtype :: String -> Array Constraint -> String -> Array Type -> Declaration
declDeriveNewtype = declDerive' CST.DeclDeriveType_Newtype

declDerive' :: CST.DeclDeriveType -> String -> Array Constraint -> String -> Array Type -> Declaration
declDerive' deriveType name constraints class' types = Declaration ado
  head <- mkInstanceHead name constraints class' types
  in CST.DeclDerive { comments: Nothing, deriveType, head }

--

newtype DataCtor =
  DataCtor (ModuleBuilder CST.DataCtor)

runDataCtor :: forall m. Monad m => DataCtor -> ModuleBuilderT m CST.DataCtor
runDataCtor (DataCtor mb) = liftModuleBuilder mb

dataCtor :: String -> Array Type -> DataCtor
dataCtor s ts = DataCtor ado
  dataCtorName <- mkName s
  dataCtorFields <- traverse runType ts
  in CST.DataCtor { dataCtorName, dataCtorFields }

mkDataHead :: String -> Array TypeVarBinding -> ModuleBuilder CST.DataHead
mkDataHead n vs = ado
  dataHdName <- mkName n
  dataHdVars <- traverse runTypeVarBinding vs
  in CST.DataHead { dataHdName, dataHdVars }

mkClassHead :: String -> Array TypeVarBinding -> Array Constraint -> Array (Array String /\ Array String) -> ModuleBuilder CST.ClassHead
mkClassHead n vs ss fds = ado
  name <- mkName n
  vars <- traverse runTypeVarBinding vs
  super <- traverse runConstraint ss
  fundeps <- Array.catMaybes <$> traverse (uncurry mkClassFundep) fds
  in { name, vars, super, fundeps }


mkClassFundep :: Array String -> Array String -> ModuleBuilder (Maybe CST.ClassFundep)
mkClassFundep is os = runMaybeT $ CST.FundepDetermines
  <$> (MaybeT $ NonEmptyArray.fromArray <$> traverse mkName is)
  <*> (MaybeT $ NonEmptyArray.fromArray <$> traverse mkName os)

mkInstanceHead :: String -> Array Constraint -> String -> Array Type -> ModuleBuilder CST.InstanceHead
mkInstanceHead name' constraints' class' types' = ado
  instName <- mkName name'
  instConstraints <- traverse runConstraint constraints'
  instClass <- mkQualName class'
  instTypes <- requireNonEmptyArray MissingInstanceHeadTypes =<< traverse runType types'
  in { instName, instConstraints, instClass, instTypes }

newtype Instance =
  Instance (ModuleBuilder CST.Instance)

runInstance :: forall m. Monad m => Instance -> ModuleBuilderT m CST.Instance
runInstance (Instance mb) = liftModuleBuilder mb

instance_ :: String -> Array Constraint -> String -> Array Type -> Array InstanceBinding -> Instance
instance_ name' constraints' class' types' body' = Instance ado
  head <- mkInstanceHead name' constraints' class' types'
  body <- traverse runInstanceBinding body'
  in { head, body }

newtype InstanceBinding =
  InstanceBinding (ModuleBuilder CST.InstanceBinding)

runInstanceBinding :: forall m. Monad m => InstanceBinding -> ModuleBuilderT m CST.InstanceBinding
runInstanceBinding (InstanceBinding mb) = liftModuleBuilder mb

instanceBSig :: String -> Type -> InstanceBinding
instanceBSig ident' type_' = InstanceBinding ado
  ident <- mkName ident'
  type_ <- runType type_'
  in CST.InstanceBindingSignature { ident, type_ }

--
instanceBName :: String -> Array Binder -> Guarded -> InstanceBinding
instanceBName name' binders' guarded' = InstanceBinding ado
  name <- mkName name'
  binders <- traverse runBinder binders'
  guarded <- runGuarded guarded'
  in CST.InstanceBindingName { name
                             , binders
                             , guarded
                             }
