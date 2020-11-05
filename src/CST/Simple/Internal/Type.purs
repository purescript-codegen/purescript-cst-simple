module CST.Simple.Internal.Type
       ( Type
       , runType
       , typ
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
       , typKinded
       , typOp
       , typConstrained
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

typ :: String -> Type
typ = typCons

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
    toRowLabel l t = runType t <#> \type_ ->
      { label: CST.Label l, type_ }

    toRowTail s = runType (typVar s)

typApp :: Type -> Array Type -> Type
typApp c as = Type $ foldl f (runType c) as
  where
    f acc' a' = CST.TypeApp
      <$> acc'
      <*> runType a'

typForall :: Array String -> Type -> Type
typForall vs t = case NonEmptyArray.fromArray vs of
  Just vs' ->
    Type $
    CST.TypeForall <$> traverse toTypeeVarName vs' <*> runType t
  Nothing ->
    t

  where
    toTypeeVarName v = CST.TypeVarName <$> mkName v

typArrow :: Type -> Type -> Type
typArrow t1 t2 = Type $ CST.TypeArr <$> runType t1 <*> runType t2

typKinded :: Type -> String -> Type
typKinded t k = Type ado
  t' <- runType t
  k' <- mkQualName k
  in CST.TypeKinded t' (CST.KindName k')

typOp :: Type -> String -> Type -> Type
typOp t1 op t2 = Type $ CST.TypeOp
  <$> runType t1
  <*> mkQualName op
  <*> runType t2

typConstrained :: Constraint -> Type -> Type
typConstrained c t = Type $ CST.TypeConstrained
  <$> runConstraint c
  <*> runType t

-- Constraint

newtype Constraint = Constraint (ModuleBuilder CST.Constraint)

runConstraint :: forall m. Monad m => Constraint -> ModuleBuilderT m CST.Constraint
runConstraint (Constraint mb) = liftModuleBuilder mb

cnst :: String -> Array Type -> Constraint
cnst s args = Constraint $ ado
  className <- mkQualName s
  args' <- traverse runType args
  in CST.Constraint
    { className
    , args: args'
    }
