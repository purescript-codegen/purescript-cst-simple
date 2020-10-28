module CST.Simple.Internal.Type
       ( Typ
       , runTyp
       , runTyp'
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
       , class AsTyp
       , asTyp
       , Constraint
       , cnst
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, addImport, addImportClass, addImportKind, addImportType, liftModuleBuilder, mkIdent, mkQualOpName, mkQualPName)
import CST.Simple.Names (QualifiedName(..), opNameToOpName, pnameToProperName)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Language.PS.CST as CST

newtype Typ = Typ (ModuleBuilder CST.Type)

runTyp :: forall m. Monad m => Typ -> ModuleBuilderT m CST.Type
runTyp (Typ mb) = liftModuleBuilder mb

typVar :: String -> Typ
typVar s = Typ $ CST.TypeVar <$> mkIdent s

typCons :: String -> Typ
typCons s = Typ do
  q@(QualifiedName { qualName }) <- mkQualPName s
  addImportType q
  pure $ CST.TypeConstructor $
    QualifiedName { qualModule: Nothing
                  , qualName: pnameToProperName qualName
                  }

typString :: String -> Typ
typString = Typ <<< pure <<< CST.TypeString

typRow :: Array (String /\ Typ) -> Maybe String -> Typ
typRow = typLabelled CST.TypeRow

typRow_ :: Array (String /\ Typ) -> Typ
typRow_ pairs = typRow pairs Nothing

typRecord :: Array (String /\ Typ) -> Maybe String -> Typ
typRecord = typLabelled CST.TypeRecord

typRecord_ :: Array (String /\ Typ) -> Typ
typRecord_ pairs = typRow pairs Nothing

typLabelled ::
  ( { rowLabels :: Array { label :: CST.Label, type_ :: CST.Type}
    , rowTail :: Maybe CST.Type
    } -> CST.Type
  ) ->
  Array (String /\ Typ) ->
  Maybe String ->
  Typ
typLabelled f pairs tailName = Typ ado
  rowLabels <- traverse (uncurry toRowLabel) pairs
  rowTail <- traverse toRowTail tailName
  in f { rowLabels, rowTail }

  where
    toRowLabel l typ = runTyp typ <#> \type_ ->
      { label: CST.Label l, type_ }

    toRowTail s = runTyp (typVar s)

typApp :: forall c a. AsTyp c => AsTyp a => c -> Array a -> Typ
typApp c as = Typ $ foldl f (runTyp (asTyp c)) as
  where
    f acc' a' = CST.TypeApp
      <$> acc'
      <*> runTyp' a'

typForall :: forall t. AsTyp t => Array String -> t -> Typ
typForall vs t = case NonEmptyArray.fromArray vs of
  Just vs' ->
    Typ $
    CST.TypeForall <$> traverse toTypeVarName vs' <*> runTyp t'
  Nothing ->
    t'

  where
    t' = asTyp t

    toTypeVarName v = CST.TypeVarName <$> mkIdent v

typArrow :: forall t1 t2. AsTyp t1 => AsTyp t2 => t1 -> t2 -> Typ
typArrow t1 t2 = Typ $ CST.TypeArr <$> runTyp' t1 <*> runTyp' t2

infixr 6 typArrow as *->

typKinded :: forall t. AsTyp t => t -> String -> Typ
typKinded t k = Typ do
  t' <- runTyp' t
  q@(QualifiedName { qualName }) <- mkQualPName k
  addImportKind q
  pure $ CST.TypeKinded t' $ CST.KindName $
    QualifiedName { qualModule: Nothing
                  , qualName: pnameToProperName qualName
                  }

infixr 8 typKinded as *::

typOp :: forall t1 t2. AsTyp t1 => AsTyp t2 => t1 -> String -> t2 -> Typ
typOp t1 op t2 = Typ do
  t1' <- runTyp' t1
  op'@(QualifiedName { qualModule, qualName }) <- map opNameToOpName <$> mkQualOpName op
  t2' <- runTyp' t2

  for_ qualModule \qm ->
    addImport qm $ CST.ImportTypeOp qualName
  let op'' = QualifiedName { qualModule: Nothing, qualName }

  pure $ CST.TypeOp t1' op'' t2'

typConstrained :: forall t. AsTyp t => Constraint -> t -> Typ
typConstrained c t = Typ $ CST.TypeConstrained
  <$> runConstraint c
  <*> runTyp' t

infixr 10 typConstrained as *=>

-- AsTyp

class AsTyp a where
  asTyp :: a -> Typ

instance asTypTyp :: AsTyp Typ where
  asTyp = identity

instance asTypString :: AsTyp String where
  asTyp = typCons

runTyp' :: forall m a. Monad m => AsTyp a => a -> ModuleBuilderT m CST.Type
runTyp' = runTyp <<< asTyp

-- Constraint

newtype Constraint = Constraint (ModuleBuilder CST.Constraint)

runConstraint :: forall m. Monad m => Constraint -> ModuleBuilderT m CST.Constraint
runConstraint (Constraint mb) = liftModuleBuilder mb

cnst :: forall t. AsTyp t => String -> Array t -> Constraint
cnst s args = Constraint do
  q@(QualifiedName { qualName }) <- mkQualPName s
  addImportClass q
  args' <- traverse runTyp' args
  pure $ CST.Constraint
    { className: QualifiedName { qualModule: Nothing
                               , qualName: pnameToProperName qualName
                               }
    , args: args'
    }
