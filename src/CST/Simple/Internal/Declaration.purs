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
       , declSignature
       , declValue
       , declInfix
       , declForeignValue
       , declForeignData
       , declForeignKind
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
       , FixityOp
       , runFixityOp
       , fixityOpValue
       , fixityOpType
       , module E
       ) where

import Prelude

import CST.Simple.Internal.Binder (Binder)
import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.Expr (Guarded, valueBindingFields)
import CST.Simple.Internal.Kind (Kind, runKind)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualConstructorName, mkQualName)
import CST.Simple.Internal.Type (Constraint, Type, runConstraint, runType)
import CST.Simple.Internal.TypeVarBinding (TypeVarBinding, runTypeVarBinding)
import CST.Simple.Internal.Utils (requireNonEmptyArray)
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Language.PS.CST (Fixity(..)) as E
import Language.PS.CST (Fixity)
import Language.PS.CST as CST

newtype Declaration =
  Declaration (ModuleBuilder CST.Declaration)

runDeclaration :: forall m. Monad m => Declaration -> ModuleBuilderT m CST.Declaration
runDeclaration (Declaration mb) = liftModuleBuilder mb

declData :: String -> Array TypeVarBinding -> Array DataCtor -> Declaration
declData name fields constructors' = Declaration ado
  head <- mkDataHead name fields
  constructors <- traverse runDataCtor constructors'
  in CST.DeclData { comments: Nothing, head, constructors }

declType :: String -> Array TypeVarBinding -> Type -> Declaration
declType name fields type_' = Declaration ado
  head <- mkDataHead name fields
  type_ <- runType type_'
  in CST.DeclType { comments: Nothing, head, type_ }

declNewtype :: String -> Array TypeVarBinding -> String -> Type -> Declaration
declNewtype dataHdName fields name' type_' = Declaration ado
  head <- mkDataHead dataHdName fields
  name <- mkName name'
  type_ <- runType type_'
  in CST.DeclNewtype { comments: Nothing, head, name, type_ }

declClass :: String -> Array TypeVarBinding -> Array Constraint -> Array (Array String /\ Array String) -> Array (String /\ Type) -> Declaration
declClass name fields super fundeps methods' = Declaration ado
  head <- mkClassHead name fields super fundeps
  methods <- traverse (uncurry mkMethod) methods'
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

declSignature :: String -> Type -> Declaration
declSignature ident' type_' = Declaration ado
  ident <- mkName ident'
  type_ <- runType type_'
  in CST.DeclSignature { comments: Nothing, ident, type_ }

declValue :: String -> Array Binder -> Guarded -> Declaration
declValue name binders guarded = Declaration ado
  fields <- valueBindingFields name binders guarded
  in CST.DeclValue { comments: Nothing
                   , valueBindingFields: fields
                   }

declInfix :: Fixity -> Int -> FixityOp -> Declaration
declInfix keyword precedence operator' = Declaration ado
  operator <- runFixityOp operator'
  in CST.DeclFixity
     { comments: Nothing
     , fixityFields:
       { keyword
       , precedence
       , operator
       }
     }

declForeignValue :: String -> Type -> Declaration
declForeignValue ident' type_' = Declaration $ cstDeclForeign <$> ado
  ident <- mkName ident'
  type_ <- runType type_'
  in CST.ForeignValue { ident, type_ }

declForeignData :: String -> Kind -> Declaration
declForeignData name' kind_' = Declaration $ cstDeclForeign <$> ado
  name <- mkName name'
  kind_ <- runKind kind_'
  in CST.ForeignData { name, kind_ }

declForeignKind :: String -> Declaration
declForeignKind name' = Declaration $ cstDeclForeign <$> ado
  name <- mkName name'
  in CST.ForeignKind { name }

cstDeclForeign :: CST.Foreign -> CST.Declaration
cstDeclForeign foreign_ =
  CST.DeclForeign { comments: Nothing, foreign_ }

--

newtype DataCtor =
  DataCtor (ModuleBuilder CST.DataCtor)

runDataCtor :: forall m. Monad m => DataCtor -> ModuleBuilderT m CST.DataCtor
runDataCtor (DataCtor mb) = liftModuleBuilder mb

dataCtor :: String -> Array Type -> DataCtor
dataCtor name fields = DataCtor ado
  dataCtorName <- mkName name
  dataCtorFields <- traverse runType fields
  in CST.DataCtor { dataCtorName, dataCtorFields }

mkDataHead :: String -> Array TypeVarBinding -> ModuleBuilder CST.DataHead
mkDataHead name vars = ado
  dataHdName <- mkName name
  dataHdVars <- traverse runTypeVarBinding vars
  in CST.DataHead { dataHdName, dataHdVars }

mkClassHead :: String -> Array TypeVarBinding -> Array Constraint -> Array (Array String /\ Array String) -> ModuleBuilder CST.ClassHead
mkClassHead name' vars' super' fundeps' = ado
  name <- mkName name'
  vars <- traverse runTypeVarBinding vars'
  super <- traverse runConstraint super'
  fundeps <- Array.catMaybes <$> traverse (uncurry mkClassFundep) fundeps'
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

instanceBName :: String -> Array Binder -> Guarded -> InstanceBinding
instanceBName name binders guarded =
  InstanceBinding $ CST.InstanceBindingName <$> valueBindingFields name binders guarded

newtype FixityOp =
  FixityOp (ModuleBuilder CST.FixityOp)

runFixityOp :: forall m. Monad m => FixityOp -> ModuleBuilderT m CST.FixityOp
runFixityOp (FixityOp mb) = liftModuleBuilder mb

fixityOpValue :: String -> String -> FixityOp
fixityOpValue name' opName' = FixityOp ado
  name <- (Left <$> mkQualName name') <|> (Right <$> mkQualConstructorName name')
  opName <- mkName opName'
  in CST.FixityValue name opName

fixityOpType :: String -> String -> FixityOp
fixityOpType name' opName' = FixityOp ado
  name <- mkQualName name'
  opName <- mkName opName'
  in CST.FixityType name opName
