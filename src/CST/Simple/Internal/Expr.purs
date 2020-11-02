module CST.Simple.Internal.Expr
       ( Expr
       , runExpr
       , exprIdent
       , exprIdentN
       , exprIdent1
       , exprIdent2
       , exprIdent3
       , exprIdent4
       , exprIdent5
       , exprIdent6
       , exprCons
       , exprConsN
       , exprCons1
       , exprCons2
       , exprCons3
       , exprCons4
       , exprCons5
       , exprCons6
       , exprBoolean
       , exprChar
       , exprString
       , exprInt
       , exprNumber
       , exprArray
       , exprRecord
       , exprTyped
       , exprOp
       , exprOpName
       , exprNegate
       , exprRecordAccess
       , exprRecordAccessN
       , exprRecordUpdate
       , exprApp
       , exprLambda
       , exprIfThenElse
       , RecordUpdate
       , runRecordUpdate
       , recordUpdate
       , recordUpdateBranch
       ) where

import Prelude

import CST.Simple.Internal.Binder (Binder, runBinder)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualName)
import CST.Simple.Internal.RecordLabeled (RecordLabeled, runRecordLabeled)
import CST.Simple.Internal.Type (Type, runType)
import CST.Simple.Names (TypedConstructorName(..))
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Language.PS.CST as CST

newtype Expr = Expr (ModuleBuilder CST.Expr)

runExpr :: forall m. Monad m => Expr -> ModuleBuilderT m CST.Expr
runExpr (Expr mb) = liftModuleBuilder mb

exprIdent :: String -> Expr
exprIdent s = Expr $ CST.ExprIdent <$> mkQualName s

exprIdentN :: String -> Array Expr -> Expr
exprIdentN s args = exprApp (exprIdent s) args

exprIdent1 :: String -> Expr -> Expr
exprIdent1 c a1 = exprIdentN c [ a1 ]

exprIdent2 :: String -> Expr -> Expr -> Expr
exprIdent2 c a1 a2 = exprIdentN c [ a1, a2 ]

exprIdent3 :: String -> Expr -> Expr -> Expr -> Expr
exprIdent3 c a1 a2 a3 = exprIdentN c [ a1, a2, a3 ]

exprIdent4 :: String -> Expr -> Expr -> Expr -> Expr -> Expr
exprIdent4 c a1 a2 a3 a4 = exprIdentN c [ a1, a2, a3, a4 ]

exprIdent5 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprIdent5 c a1 a2 a3 a4 a5 = exprIdentN c [ a1, a2, a3, a4, a5 ]

exprIdent6 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprIdent6 c a1 a2 a3 a4 a5 a6 = exprIdentN c [ a1, a2, a3, a4, a5, a6 ]

exprCons :: String -> Expr
exprCons c = Expr $ CST.ExprConstructor <$> (qualifiedCons <|> unqualifiedCons)
  where
    qualifiedCons =
      map getNamePart <$> mkQualName c

    unqualifiedCons = mkName c <#> \name ->
      CST.QualifiedName { qualModule: Nothing
                        , qualName: name
                        }

    getNamePart (TypedConstructorName _ n) = n

exprConsN :: String -> Array Expr -> Expr
exprConsN c args = exprApp (exprCons c) args

exprCons1 :: String -> Expr -> Expr
exprCons1 c a1 = exprConsN c [ a1 ]

exprCons2 :: String -> Expr -> Expr -> Expr
exprCons2 c a1 a2 = exprConsN c [ a1, a2 ]

exprCons3 :: String -> Expr -> Expr -> Expr -> Expr
exprCons3 c a1 a2 a3 = exprConsN c [ a1, a2, a3 ]

exprCons4 :: String -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons4 c a1 a2 a3 a4 = exprConsN c [ a1, a2, a3, a4 ]

exprCons5 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons5 c a1 a2 a3 a4 a5 = exprConsN c [ a1, a2, a3, a4, a5 ]

exprCons6 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons6 c a1 a2 a3 a4 a5 a6 = exprConsN c [ a1, a2, a3, a4, a5, a6 ]

exprBoolean :: Boolean -> Expr
exprBoolean = Expr <<< pure <<< CST.ExprBoolean

exprChar :: Char -> Expr
exprChar = Expr <<< pure <<< CST.ExprChar

exprString :: String -> Expr
exprString = Expr <<< pure <<< CST.ExprString

exprInt :: Int -> Expr
exprInt = Expr <<< pure <<< CST.ExprNumber <<< Left

exprNumber :: Number -> Expr
exprNumber = Expr <<< pure <<< CST.ExprNumber <<< Right

exprArray :: Array Expr -> Expr
exprArray es =
  Expr $ CST.ExprArray <$> traverse runExpr es

exprApp :: Expr -> Array Expr -> Expr
exprApp i args = Expr $ foldl appExpr (runExpr i) args
  where
    appExpr a bExpr =
      CST.ExprApp <$> a <*> runExpr bExpr

exprRecord :: Array (RecordLabeled Expr) -> Expr
exprRecord ls =
  Expr $ CST.ExprRecord <$> traverse run ls
  where
    run rl = do
      rl' <- runRecordLabeled rl
      traverse runExpr rl'

exprTyped :: Expr -> Type -> Expr
exprTyped e t =
  Expr $ CST.ExprTyped <$> runExpr e <*> runType t

exprOp :: Expr -> String -> Expr -> Expr
exprOp e1 opStr e2 =
  Expr $ CST.ExprOp
  <$> runExpr e1
  <*> mkQualName opStr
  <*> runExpr e2

exprOpName :: String -> Expr
exprOpName opStr =
  Expr $ CST.ExprOpName
  <$> mkQualName opStr

exprNegate :: Expr -> Expr
exprNegate e =
  Expr $ CST.ExprNegate <$> runExpr e

exprRecordAccess :: Expr -> String -> Expr
exprRecordAccess e p =
  exprRecordAccessN e (String.split (String.Pattern ".") p)

exprRecordAccessN :: Expr -> Array String -> Expr
exprRecordAccessN e p =
  case NonEmptyArray.fromArray p of
    Just p' ->
      Expr ado
      recExpr <- runExpr e
      in CST.ExprRecordAccessor
         { recExpr
         , recPath: CST.Label <$> p'
         }
    Nothing ->
      e

exprRecordUpdate :: Expr -> Array RecordUpdate -> Expr
exprRecordUpdate e es = Expr ado
  e' <- runExpr e
  es' <- runRecordUpdates es
  in foldl CST.ExprRecordUpdate e' es'

-- See CST.Simple.((*->))
exprLambda :: Array Binder -> Expr -> Expr
exprLambda bs b = case NonEmptyArray.fromArray bs of
  Just bs' -> Expr ado
    binders <- traverse runBinder bs'
    body <- runExpr b
    in CST.ExprLambda { binders, body }
  Nothing ->
    b

exprIfThenElse :: Expr -> Expr -> Expr -> Expr
exprIfThenElse c t_ f_ = Expr ado
  cond <- runExpr c
  true_ <- runExpr t_
  false_ <- runExpr f_
  in CST.ExprIf { cond, true_, false_ }

-- record update

newtype RecordUpdate =
  RecordUpdate (ModuleBuilder (Maybe CST.RecordUpdate))

runRecordUpdate :: forall m. Monad m => RecordUpdate -> ModuleBuilderT m (Maybe CST.RecordUpdate)
runRecordUpdate (RecordUpdate mb) =
  liftModuleBuilder mb

runRecordUpdates :: forall m. Monad m => Array RecordUpdate -> ModuleBuilderT m (Maybe (NonEmptyArray CST.RecordUpdate))
runRecordUpdates es =
  NonEmptyArray.fromArray
  <<< Array.catMaybes
  <$> traverse runRecordUpdate es

recordUpdate :: String -> Expr -> RecordUpdate
recordUpdate l expr =
  RecordUpdate $ Just <<< CST.RecordUpdateLeaf (CST.Label l) <$> runExpr expr

recordUpdateBranch :: String -> Array RecordUpdate -> RecordUpdate
recordUpdateBranch l es =
  RecordUpdate
  $ map (CST.RecordUpdateBranch (CST.Label l))
  <$> runRecordUpdates es
