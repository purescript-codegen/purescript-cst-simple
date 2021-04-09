module CST.Simple.Internal.Type
       ( Type
       , runType
       , typ
       , typVar
       , typCons
       , typConsN
       , typCons1
       , typCons2
       , typCons3
       , typCons4
       , typCons5
       , typCons6
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
       , runConstraint
       , cnst
       , cnstN
       , cnst1
       , cnst2
       , cnst3
       , cnst4
       , cnst5
       , cnst6
       ) where

import Prelude

import CST.Simple.Internal.Kind (Kind, runKind)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, mkName, mkQualName)
import CST.Simple.Internal.TypeVarBinding (TypeVarBinding, runTypeVarBinding)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Language.PS.CST as CST

newtype Type = Type (ModuleBuilder CST.PSType)

derive newtype instance typeEq :: Eq Type
derive newtype instance typeOrd :: Ord Type

instance typeShow :: Show Type where
  show (Type mb) = "(Type " <> show mb <> ")"

runType :: Type -> ModuleBuilder CST.PSType
runType (Type mb) = mb

typ :: String -> Type
typ = typCons

typVar :: String -> Type
typVar ident = Type $ CST.TypeVar <$> mkName ident

typCons :: String -> Type
typCons cons =
  Type $ CST.TypeConstructor <$> mkQualName cons

typConsN :: String -> Array Type -> Type
typConsN cons args =
  typApp (typCons cons) args

typCons1 :: String -> Type -> Type
typCons1 cons a1 =
  typConsN cons [ a1 ]

typCons2 :: String -> Type -> Type -> Type
typCons2 cons a1 a2 =
  typConsN cons [ a1, a2 ]

typCons3 :: String -> Type -> Type -> Type -> Type
typCons3 cons a1 a2 a3 =
  typConsN cons [ a1, a2, a3 ]

typCons4 :: String -> Type -> Type -> Type -> Type -> Type
typCons4 cons a1 a2 a3 a4 =
  typConsN cons [ a1, a2, a3, a4 ]

typCons5 :: String -> Type -> Type -> Type -> Type -> Type -> Type
typCons5 cons a1 a2 a3 a4 a5 =
  typConsN cons [ a1, a2, a3, a4, a5 ]

typCons6 :: String -> Type -> Type -> Type -> Type -> Type -> Type -> Type
typCons6 cons a1 a2 a3 a4 a5 a6 =
  typConsN cons [ a1, a2, a3, a4, a5, a6 ]

typString :: String -> Type
typString = Type <<< pure <<< CST.TypeString

typRow :: Array (String /\ Type) -> Maybe Type -> Type
typRow = typLabelled CST.TypeRow

typRow_ :: Array (String /\ Type) -> Type
typRow_ pairs = typRow pairs Nothing

typRecord :: Array (String /\ Type) -> Maybe Type -> Type
typRecord = typLabelled CST.TypeRecord

typRecord_ :: Array (String /\ Type) -> Type
typRecord_ pairs = typRecord pairs Nothing

typLabelled ::
  ( { rowLabels :: Array { label :: CST.Label, type_ :: CST.PSType}
    , rowTail :: Maybe CST.PSType
    } -> CST.PSType
  ) ->
  Array (String /\ Type) ->
  Maybe Type ->
  Type
typLabelled f pairs rowTail' = Type ado
  rowLabels <- traverse (uncurry toRowLabel) pairs
  rowTail <- traverse runType rowTail'
  in f { rowLabels, rowTail }

  where
    toRowLabel l t = runType t <#> \type_ ->
      { label: CST.Label l, type_ }

typApp :: Type -> Array Type -> Type
typApp cons args = Type $ foldl f (runType cons) args
  where
    f acc' a' = CST.TypeApp
      <$> acc'
      <*> runType a'

typForall :: Array TypeVarBinding -> Type -> Type
typForall vars' type_ = case NonEmptyArray.fromArray vars' of
  Just vars ->
    Type $
    CST.TypeForall <$> traverse runTypeVarBinding vars <*> runType type_
  Nothing ->
    type_

typArrow :: Type -> Type -> Type
typArrow t1 t2 = Type $ CST.TypeArr <$> runType t1 <*> runType t2

typKinded :: Type -> Kind -> Type
typKinded type_ kind_ = Type $ CST.TypeKinded
  <$> runType type_
  <*> runKind kind_

typOp :: Type -> String -> Type -> Type
typOp t1 op t2 = Type $ CST.TypeOp
  <$> runType t1
  <*> mkQualName op
  <*> runType t2

typConstrained :: Constraint -> Type -> Type
typConstrained constraint type_ = Type $ CST.TypeConstrained
  <$> runConstraint constraint
  <*> runType type_

-- Constraint

newtype Constraint = Constraint (ModuleBuilder CST.PSConstraint)

derive newtype instance constraintEq :: Eq Constraint
derive newtype instance constraintOrd :: Ord Constraint

instance constraintShow :: Show Constraint where
  show (Constraint mb) =
    "(Constraint " <> show mb <> ")"

runConstraint :: Constraint -> ModuleBuilder CST.PSConstraint
runConstraint (Constraint mb) = mb

cnst :: String -> Array Type -> Constraint
cnst className' args' = Constraint $ ado
  className <- mkQualName className'
  args <- traverse runType args'
  in CST.PSConstraint
    { className
    , args
    }

cnstN :: String -> Array Type -> Constraint
cnstN = cnst

cnst1 :: String -> Type -> Constraint
cnst1 className a1 =
  cnstN className [ a1 ]

cnst2 :: String -> Type -> Type -> Constraint
cnst2 className a1 a2 =
  cnstN className [ a1, a2 ]

cnst3 :: String -> Type -> Type -> Type -> Constraint
cnst3 className a1 a2 a3 =
  cnstN className [ a1, a2, a3 ]

cnst4 :: String -> Type -> Type -> Type -> Type -> Constraint
cnst4 className a1 a2 a3 a4 =
  cnstN className [ a1, a2, a3, a4 ]

cnst5 :: String -> Type -> Type -> Type -> Type -> Type -> Constraint
cnst5 className a1 a2 a3 a4 a5 =
  cnstN className [ a1, a2, a3, a4, a5 ]

cnst6 :: String -> Type -> Type -> Type -> Type -> Type -> Type -> Constraint
cnst6 className a1 a2 a3 a4 a5 a6 =
  cnstN className [ a1, a2, a3, a4, a5, a6 ]
