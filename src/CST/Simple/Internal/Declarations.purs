module CST.Simple.Internal.Declaration
       ( Declaration
       , runDeclaration
       , declData
       , declType
       , declNewtype
       , DataCtor
       , dataCtor
       , runDataCtor
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName)
import CST.Simple.Internal.Type (Type, runType)
import CST.Simple.Internal.TypeVarBinding (TypeVarBinding, runTypeVarBinding)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Language.PS.CST as CST

newtype Declaration =
  Declaration (ModuleBuilder CST.Declaration)

runDeclaration :: forall m. Monad m => Declaration -> ModuleBuilderT m CST.Declaration
runDeclaration (Declaration mb) = liftModuleBuilder mb

declData :: String -> Array TypeVarBinding -> Array DataCtor -> Declaration
declData n vs cs = Declaration ado
  head <- mkDHead n vs
  constructors <- traverse runDataCtor cs
  in CST.DeclData { comments: Nothing, head, constructors }

declType :: String -> Array TypeVarBinding -> Type -> Declaration
declType n vs t = Declaration ado
  head <- mkDHead n vs
  type_ <- runType t
  in CST.DeclType { comments: Nothing, head, type_ }

declNewtype :: String -> Array TypeVarBinding -> String -> Type -> Declaration
declNewtype n vs c t = Declaration ado
  head <- mkDHead n vs
  name <- mkName c
  type_ <- runType t
  in CST.DeclNewtype { comments: Nothing, head, name, type_ }

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

mkDHead :: String -> Array TypeVarBinding -> ModuleBuilder CST.DataHead
mkDHead n vs = ado
  dataHdName <- mkName n
  dataHdVars <- traverse runTypeVarBinding vs
  in CST.DataHead { dataHdName, dataHdVars }
