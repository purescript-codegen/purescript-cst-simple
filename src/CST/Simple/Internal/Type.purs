module CST.Simple.Internal.Type
       ( Type
       , runType
       , runType'
       , typVar
       , typCons
       , typString
       , typRow
       , typRow_
       , typRecord
       , typRecord_
       , typApp
       , typForall
       , typArrow
       , (*->)
       , typKinded
       , (*::)
       , typOp
       , typConstrained
       , (*=>)
       , class AsType
       , asType
       , Constraint
       , cnst
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualName)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Language.PS.CST as CST

newtype Type = Type (ModuleBuilder CST.Type)

runType :: forall m. Monad m => Type -> ModuleBuilderT m CST.Type
runType (Type mb) = liftModuleBuilder mb

typVar :: String -> Type
typVar s = Type $ CST.TypeVar <$> mkName s

typCons :: String -> Type
typCons s =
  Type $ CST.TypeConstructor <$> mkQualName s

typString :: String -> Type
typString = Type <<< pure <<< CST.TypeString

typRow :: Array (String /\ Type) -> Maybe String -> Type
typRow = typLabelled CST.TypeRow

typRow_ :: Array (String /\ Type) -> Type
typRow_ pairs = typRow pairs Nothing

typRecord :: Array (String /\ Type) -> Maybe String -> Type
typRecord = typLabelled CST.TypeRecord

typRecord_ :: Array (String /\ Type) -> Type
typRecord_ pairs = typRow pairs Nothing

typLabelled ::
  ( { rowLabels :: Array { label :: CST.Label, type_ :: CST.Type}
    , rowTail :: Maybe CST.Type
    } -> CST.Type
  ) ->
  Array (String /\ Type) ->
  Maybe String ->
  Type
typLabelled f pairs tailName = Type ado
  rowLabels <- traverse (uncurry toRowLabel) pairs
  rowTail <- traverse toRowTail tailName
  in f { rowLabels, rowTail }

  where
    toRowLabel l typ = runType typ <#> \type_ ->
      { label: CST.Label l, type_ }

    toRowTail s = runType (typVar s)

typApp :: forall c a. AsType c => AsType a => c -> Array a -> Type
typApp c as = Type $ foldl f (runType (asType c)) as
  where
    f acc' a' = CST.TypeApp
      <$> acc'
      <*> runType' a'

typForall :: forall t. AsType t => Array String -> t -> Type
typForall vs t = case NonEmptyArray.fromArray vs of
  Just vs' ->
    Type $
    CST.TypeForall <$> traverse toTypeeVarName vs' <*> runType t'
  Nothing ->
    t'

  where
    t' = asType t

    toTypeeVarName v = CST.TypeVarName <$> mkName v

typArrow :: forall t1 t2. AsType t1 => AsType t2 => t1 -> t2 -> Type
typArrow t1 t2 = Type $ CST.TypeArr <$> runType' t1 <*> runType' t2

infixr 6 typArrow as *->

typKinded :: forall t. AsType t => t -> String -> Type
typKinded t k = Type ado
  t' <- runType' t
  k' <- mkQualName k
  in CST.TypeKinded t' (CST.KindName k')

infixr 8 typKinded as *::

typOp :: forall t1 t2. AsType t1 => AsType t2 => t1 -> String -> t2 -> Type
typOp t1 op t2 = Type $ CST.TypeOp
  <$> runType' t1
  <*> mkQualName op
  <*> runType' t2

typConstrained :: forall t. AsType t => Constraint -> t -> Type
typConstrained c t = Type $ CST.TypeConstrained
  <$> runConstraint c
  <*> runType' t

infixr 10 typConstrained as *=>

-- AsType

class AsType a where
  asType :: a -> Type

instance asTypeType :: AsType Type where
  asType = identity

instance asTypeString :: AsType String where
  asType = typCons

runType' :: forall m a. Monad m => AsType a => a -> ModuleBuilderT m CST.Type
runType' = runType <<< asType

-- Constraint

newtype Constraint = Constraint (ModuleBuilder CST.Constraint)

runConstraint :: forall m. Monad m => Constraint -> ModuleBuilderT m CST.Constraint
runConstraint (Constraint mb) = liftModuleBuilder mb

cnst :: forall t. AsType t => String -> Array t -> Constraint
cnst s args = Constraint $ ado
  className <- mkQualName s
  args' <- traverse runType' args
  in CST.Constraint
    { className
    , args: args'
    }
