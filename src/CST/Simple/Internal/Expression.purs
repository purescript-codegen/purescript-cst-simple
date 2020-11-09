module CST.Simple.Internal.Expression
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
       , exprCaseOfN
       , exprCaseOf1
       , exprCaseOf2
       , exprCaseOf3
       , exprCaseOf4
       , exprCaseOf5
       , exprCaseOf6
       , exprLetIn
       , exprDo
       , exprAdo
       , RecordUpdate
       , runRecordUpdate
       , recordUpdate
       , recordUpdateBranch
       , CaseOfBranch
       , runCaseOfBranch
       , caseOfBranchN
       , caseOfBranch1
       , caseOfBranch2
       , caseOfBranch3
       , caseOfBranch4
       , caseOfBranch5
       , caseOfBranch6
       , LetBinding
       , runLetBinding
       , letSig
       , letName
       , letPattern
       , Where
       , runWhere
       , whr_
       , whr
       , Guarded
       , runGuarded
       , grd_
       , grdUncond
       , DoStatement
       , runDoStatement
       , doLet
       , doDiscard
       , doBind
       , valueBindingFields
       ) where

import Prelude

import CST.Simple.Internal.Binder (Binder, runBinder)
import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualConstructorName, mkQualName)
import CST.Simple.Internal.RecordLabeled (RecordLabeled, runRecordLabeled)
import CST.Simple.Internal.Type (Type, runType)
import CST.Simple.Internal.Utils (noteM)
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
exprCons c = Expr $ CST.ExprConstructor <$> mkQualConstructorName c

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

exprCaseOfN :: Array Expr -> Array CaseOfBranch -> Expr
exprCaseOfN es bs = Expr ado
  head <- noteM MissingCaseOfHeadBinders =<< NonEmptyArray.fromArray <$> traverse runExpr es
  branches <- noteM MissingCaseOfBranches =<< NonEmptyArray.fromArray <$> traverse runCaseOfBranch bs
  in CST.ExprCase { head, branches }

exprCaseOf1 :: Expr -> Array CaseOfBranch -> Expr
exprCaseOf1 e1 = exprCaseOfN [ e1 ]

exprCaseOf2 :: Expr -> Expr -> Array CaseOfBranch -> Expr
exprCaseOf2 e1 e2 = exprCaseOfN [ e1, e2 ]

exprCaseOf3 :: Expr -> Expr -> Expr -> Array CaseOfBranch -> Expr
exprCaseOf3 e1 e2 e3 = exprCaseOfN [ e1, e2, e3 ]

exprCaseOf4 :: Expr -> Expr -> Expr -> Expr -> Array CaseOfBranch -> Expr
exprCaseOf4 e1 e2 e3 e4 = exprCaseOfN [ e1, e2, e3, e4 ]

exprCaseOf5 :: Expr -> Expr -> Expr -> Expr -> Expr -> Array CaseOfBranch -> Expr
exprCaseOf5 e1 e2 e3 e4 e5 = exprCaseOfN [ e1, e2, e3, e4, e5 ]

exprCaseOf6 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Array CaseOfBranch -> Expr
exprCaseOf6 e1 e2 e3 e4 e5 e6 = exprCaseOfN [ e1, e2, e3, e4, e5, e6 ]

exprLetIn :: Array LetBinding -> Expr -> Expr
exprLetIn lbs e = case NonEmptyArray.fromArray lbs of
  Just lbs' -> Expr ado
    bindings <- traverse runLetBinding lbs'
    body <- runExpr e
    in CST.ExprLet { bindings, body }
  Nothing ->
    e

exprDo :: Array DoStatement -> Expr
exprDo ds = Expr
  $ noteM MissingDoStatements
    =<< map CST.ExprDo
    <<< NonEmptyArray.fromArray
    <<< Array.catMaybes
    <$> traverse runDoStatement ds

exprAdo :: Array DoStatement -> Expr -> Expr
exprAdo ds r = Expr ado
  statements <- Array.catMaybes <$> traverse runDoStatement ds
  result <- runExpr r
  in CST.ExprAdo { statements, result }

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

-- case branch

newtype CaseOfBranch =
  CaseOfBranch (ModuleBuilder { binders :: NonEmptyArray CST.Binder
                              , body :: CST.Guarded
                              }
               )

runCaseOfBranch ::
  forall m. Monad m =>
  CaseOfBranch ->
  ModuleBuilderT m { binders :: NonEmptyArray CST.Binder
                   , body :: CST.Guarded
                   }
runCaseOfBranch (CaseOfBranch mb) = liftModuleBuilder mb

-- See (*->)
caseOfBranchN :: Array Binder -> Expr -> CaseOfBranch
caseOfBranchN bis b = CaseOfBranch ado
  binders <- noteM MissingCaseOfBranchBinders =<< runBinders
  body <- mkBody
  in { binders, body }

  where
    runBinders = NonEmptyArray.fromArray <$> traverse runBinder bis
    mkBody = runExpr b <#> \expr -> CST.Unconditional { expr, whereBindings: [] }


caseOfBranch1 :: Binder -> Expr -> CaseOfBranch
caseOfBranch1 bi1 b = caseOfBranchN [ bi1 ] b

caseOfBranch2 :: Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch2 bi1 bi2 b = caseOfBranchN [ bi1, bi2 ] b

caseOfBranch3 :: Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch3 bi1 bi2 bi3 b = caseOfBranchN [ bi1, bi2, bi3 ] b

caseOfBranch4 :: Binder -> Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch4   bi1 bi2 bi3 bi4 b = caseOfBranchN [ bi1, bi2, bi3, bi4 ] b

caseOfBranch5 :: Binder -> Binder -> Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch5  bi1 bi2 bi3 bi4 bi5 b = caseOfBranchN [ bi1, bi2, bi3, bi4, bi5 ] b

caseOfBranch6 :: Binder -> Binder -> Binder -> Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch6 bi1 bi2 bi3 bi4 bi5 bi6 b = caseOfBranchN [ bi1, bi2, bi3, bi4, bi5, bi6 ] b

-- let binding

newtype LetBinding =
  LetBinding (ModuleBuilder CST.LetBinding)

runLetBinding :: forall m. Monad m => LetBinding -> ModuleBuilderT m CST.LetBinding
runLetBinding (LetBinding mb) = liftModuleBuilder mb

letSig :: String -> Type -> LetBinding
letSig i t = LetBinding ado
  ident <- mkName i
  type_ <- runType t
  in CST.LetBindingSignature { ident, type_ }

letName :: String -> Array Binder -> Guarded -> LetBinding
letName name binders guarded =
  LetBinding $ CST.LetBindingName <$> valueBindingFields name binders guarded

letPattern :: Binder -> Where -> LetBinding
letPattern b w = LetBinding ado
  binder <- runBinder b
  where_ <- runWhere w
  in CST.LetBindingPattern { binder, where_ }

-- where

newtype Where =
  Where (ModuleBuilder CST.Where)

runWhere :: forall m. Monad m => Where -> ModuleBuilderT m CST.Where
runWhere (Where mb) = liftModuleBuilder mb

whr :: Expr -> Array LetBinding -> Where
whr e bs = Where ado
  expr <- runExpr e
  whereBindings <- traverse runLetBinding bs
  in { expr, whereBindings }

whr_ :: Expr -> Where
whr_ e = whr e []

-- guarded

newtype Guarded =
  Guarded (ModuleBuilder CST.Guarded)

runGuarded :: forall m. Monad m => Guarded -> ModuleBuilderT m CST.Guarded
runGuarded (Guarded mb) = liftModuleBuilder mb

grd_ :: Expr -> Guarded
grd_ = grdUncond <<< whr_

grdUncond :: Where -> Guarded
grdUncond w = Guarded $ CST.Unconditional <$> runWhere w

-- todo grdGuarded

-- do statement

newtype DoStatement =
  DoStatement (ModuleBuilder (Maybe CST.DoStatement))

runDoStatement :: forall m. Monad m => DoStatement -> ModuleBuilderT m (Maybe CST.DoStatement)
runDoStatement (DoStatement mb) = liftModuleBuilder mb

doLet :: Array LetBinding -> DoStatement
doLet lbs = DoStatement case NonEmptyArray.fromArray lbs of
  Just lbs' ->
    Just <<< CST.DoLet <$> traverse runLetBinding lbs'
  Nothing ->
    pure Nothing

doDiscard :: Expr -> DoStatement
doDiscard =
  DoStatement <<< map (Just <<< CST.DoDiscard) <<< runExpr

doBind :: Binder -> Expr -> DoStatement
doBind b e = DoStatement ado
  binder <- runBinder b
  expr <- runExpr e
  in Just $ CST.DoBind { binder, expr }

-- value binding fields

valueBindingFields :: String -> Array Binder -> Guarded -> ModuleBuilder CST.ValueBindingFields
valueBindingFields name' binders' guarded' = ado
  name <- mkName name'
  binders <- traverse runBinder binders'
  guarded <- runGuarded guarded'
  in { name, binders, guarded }
