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
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, mkName, mkQualConstructorName, mkQualName)
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

runExpr :: Expr -> ModuleBuilder CST.Expr
runExpr (Expr mb) = mb

exprIdent :: String -> Expr
exprIdent ident = Expr $ CST.ExprIdent <$> mkQualName ident

exprIdentN :: String -> Array Expr -> Expr
exprIdentN ident args = exprApp (exprIdent ident) args

exprIdent1 :: String -> Expr -> Expr
exprIdent1 ident a1 = exprIdentN ident [ a1 ]

exprIdent2 :: String -> Expr -> Expr -> Expr
exprIdent2 ident a1 a2 = exprIdentN ident [ a1, a2 ]

exprIdent3 :: String -> Expr -> Expr -> Expr -> Expr
exprIdent3 ident a1 a2 a3 = exprIdentN ident [ a1, a2, a3 ]

exprIdent4 :: String -> Expr -> Expr -> Expr -> Expr -> Expr
exprIdent4 ident a1 a2 a3 a4 = exprIdentN ident [ a1, a2, a3, a4 ]

exprIdent5 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprIdent5 ident a1 a2 a3 a4 a5 = exprIdentN ident [ a1, a2, a3, a4, a5 ]

exprIdent6 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprIdent6 ident a1 a2 a3 a4 a5 a6 = exprIdentN ident [ a1, a2, a3, a4, a5, a6 ]

exprCons :: String -> Expr
exprCons cons = Expr $ CST.ExprConstructor <$> mkQualConstructorName cons

exprConsN :: String -> Array Expr -> Expr
exprConsN cons args = exprApp (exprCons cons) args

exprCons1 :: String -> Expr -> Expr
exprCons1 cons a1 = exprConsN cons [ a1 ]

exprCons2 :: String -> Expr -> Expr -> Expr
exprCons2 cons a1 a2 = exprConsN cons [ a1, a2 ]

exprCons3 :: String -> Expr -> Expr -> Expr -> Expr
exprCons3 cons a1 a2 a3 = exprConsN cons [ a1, a2, a3 ]

exprCons4 :: String -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons4 cons a1 a2 a3 a4 = exprConsN cons [ a1, a2, a3, a4 ]

exprCons5 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons5 cons a1 a2 a3 a4 a5 = exprConsN cons [ a1, a2, a3, a4, a5 ]

exprCons6 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons6 cons a1 a2 a3 a4 a5 a6 = exprConsN cons [ a1, a2, a3, a4, a5, a6 ]

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
exprArray exprs =
  Expr $ CST.ExprArray <$> traverse runExpr exprs

exprApp :: Expr -> Array Expr -> Expr
exprApp i args = Expr $ foldl appExpr (runExpr i) args
  where
    appExpr a bExpr =
      CST.ExprApp <$> a <*> runExpr bExpr

exprRecord :: Array (RecordLabeled Expr) -> Expr
exprRecord entries =
  Expr $ CST.ExprRecord <$> traverse run entries
  where
    run rl = do
      rl' <- runRecordLabeled rl
      traverse runExpr rl'

exprTyped :: Expr -> Type -> Expr
exprTyped expr type_ =
  Expr $ CST.ExprTyped <$> runExpr expr <*> runType type_

exprOp :: Expr -> String -> Expr -> Expr
exprOp expr1 opStr expr2 =
  Expr $ CST.ExprOp
  <$> runExpr expr1
  <*> mkQualName opStr
  <*> runExpr expr2

exprOpName :: String -> Expr
exprOpName op =
  Expr $ CST.ExprOpName
  <$> mkQualName op

exprNegate :: Expr -> Expr
exprNegate expr =
  Expr $ CST.ExprNegate <$> runExpr expr

exprRecordAccess :: Expr -> String -> Expr
exprRecordAccess e p =
  exprRecordAccessN e (String.split (String.Pattern ".") p)

exprRecordAccessN :: Expr -> Array String -> Expr
exprRecordAccessN expr recPaths' =
  case NonEmptyArray.fromArray recPaths' of
    Just recPaths ->
      Expr ado
      recExpr <- runExpr expr
      in CST.ExprRecordAccessor
         { recExpr
         , recPath: CST.Label <$> recPaths
         }
    Nothing ->
      expr

exprRecordUpdate :: Expr -> Array RecordUpdate -> Expr
exprRecordUpdate expr' updates' = Expr ado
  expr <- runExpr expr'
  updates <- runRecordUpdates updates'
  in foldl CST.ExprRecordUpdate expr updates

-- See CST.Simple.((*->))
exprLambda :: Array Binder -> Expr -> Expr
exprLambda binders' body' = case NonEmptyArray.fromArray binders' of
  Just bs' -> Expr ado
    binders <- traverse runBinder bs'
    body <- runExpr body'
    in CST.ExprLambda { binders, body }
  Nothing ->
    body'

exprIfThenElse :: Expr -> Expr -> Expr -> Expr
exprIfThenElse cond' true_' false_' = Expr ado
  cond <- runExpr cond'
  true_ <- runExpr true_'
  false_ <- runExpr false_'
  in CST.ExprIf { cond, true_, false_ }

exprCaseOfN :: Array Expr -> Array CaseOfBranch -> Expr
exprCaseOfN head' branches' = Expr ado
  head <- noteM MissingCaseOfHeadBinders =<< NonEmptyArray.fromArray <$> traverse runExpr head'
  branches <- noteM MissingCaseOfBranches =<< NonEmptyArray.fromArray <$> traverse runCaseOfBranch branches'
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
exprLetIn letBindings e = case NonEmptyArray.fromArray letBindings of
  Just letBindings' -> Expr ado
    bindings <- traverse runLetBinding letBindings'
    body <- runExpr e
    in CST.ExprLet { bindings, body }
  Nothing ->
    e

exprDo :: Array DoStatement -> Expr
exprDo doStatments = Expr
  $ noteM MissingDoStatements
    =<< map CST.ExprDo
    <<< NonEmptyArray.fromArray
    <<< Array.catMaybes
    <$> traverse runDoStatement doStatments

exprAdo :: Array DoStatement -> Expr -> Expr
exprAdo statements' result' = Expr ado
  statements <- Array.catMaybes <$> traverse runDoStatement statements'
  result <- runExpr result'
  in CST.ExprAdo { statements, result }

-- record update

newtype RecordUpdate =
  RecordUpdate (ModuleBuilder (Maybe CST.RecordUpdate))

runRecordUpdate :: RecordUpdate -> ModuleBuilder (Maybe CST.RecordUpdate)
runRecordUpdate (RecordUpdate mb) =
  mb

runRecordUpdates :: Array RecordUpdate -> ModuleBuilder (Maybe (NonEmptyArray CST.RecordUpdate))
runRecordUpdates es =
  NonEmptyArray.fromArray
  <<< Array.catMaybes
  <$> traverse runRecordUpdate es

recordUpdate :: String -> Expr -> RecordUpdate
recordUpdate label expr =
  RecordUpdate $ Just <<< CST.RecordUpdateLeaf (CST.Label label) <$> runExpr expr

recordUpdateBranch :: String -> Array RecordUpdate -> RecordUpdate
recordUpdateBranch label updates =
  RecordUpdate
  $ map (CST.RecordUpdateBranch (CST.Label label))
  <$> runRecordUpdates updates

-- case branch

newtype CaseOfBranch =
  CaseOfBranch (ModuleBuilder { binders :: NonEmptyArray CST.Binder
                              , body :: CST.Guarded
                              }
               )

runCaseOfBranch ::
  CaseOfBranch ->
  ModuleBuilder { binders :: NonEmptyArray CST.Binder
                , body :: CST.Guarded
                }
runCaseOfBranch (CaseOfBranch mb) = mb

-- See (*->)
caseOfBranchN :: Array Binder -> Expr -> CaseOfBranch
caseOfBranchN binders' expr' = CaseOfBranch ado
  binders <- noteM MissingCaseOfBranchBinders =<< runBinders
  body <- mkBody
  in { binders, body }

  where
    runBinders = NonEmptyArray.fromArray <$> traverse runBinder binders'
    mkBody = runExpr expr' <#> \expr -> CST.Unconditional { expr, whereBindings: [] }


caseOfBranch1 :: Binder -> Expr -> CaseOfBranch
caseOfBranch1 bi1 b = caseOfBranchN [ bi1 ] b

caseOfBranch2 :: Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch2 bi1 bi2 b = caseOfBranchN [ bi1, bi2 ] b

caseOfBranch3 :: Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch3 bi1 bi2 bi3 b = caseOfBranchN [ bi1, bi2, bi3 ] b

caseOfBranch4 :: Binder -> Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch4 bi1 bi2 bi3 bi4 b = caseOfBranchN [ bi1, bi2, bi3, bi4 ] b

caseOfBranch5 :: Binder -> Binder -> Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch5 bi1 bi2 bi3 bi4 bi5 b = caseOfBranchN [ bi1, bi2, bi3, bi4, bi5 ] b

caseOfBranch6 :: Binder -> Binder -> Binder -> Binder -> Binder -> Binder -> Expr -> CaseOfBranch
caseOfBranch6 bi1 bi2 bi3 bi4 bi5 bi6 b = caseOfBranchN [ bi1, bi2, bi3, bi4, bi5, bi6 ] b

-- let binding

newtype LetBinding =
  LetBinding (ModuleBuilder CST.LetBinding)

runLetBinding :: LetBinding -> ModuleBuilder CST.LetBinding
runLetBinding (LetBinding mb) = mb

letSig :: String -> Type -> LetBinding
letSig ident' type_' = LetBinding ado
  ident <- mkName ident'
  type_ <- runType type_'
  in CST.LetBindingSignature { ident, type_ }

letName :: String -> Array Binder -> Guarded -> LetBinding
letName name binders guarded =
  LetBinding $ CST.LetBindingName <$> valueBindingFields name binders guarded

letPattern :: Binder -> Where -> LetBinding
letPattern binder' where_' = LetBinding ado
  binder <- runBinder binder'
  where_ <- runWhere where_'
  in CST.LetBindingPattern { binder, where_ }

-- where

newtype Where =
  Where (ModuleBuilder CST.Where)

runWhere :: Where -> ModuleBuilder CST.Where
runWhere (Where mb) = mb

whr :: Expr -> Array LetBinding -> Where
whr expr' whereBindings' = Where ado
  expr <- runExpr expr'
  whereBindings <- traverse runLetBinding whereBindings'
  in { expr, whereBindings }

whr_ :: Expr -> Where
whr_ expr = whr expr []

-- guarded

newtype Guarded =
  Guarded (ModuleBuilder CST.Guarded)

runGuarded :: Guarded -> ModuleBuilder CST.Guarded
runGuarded (Guarded mb) = mb

grd_ :: Expr -> Guarded
grd_ = grdUncond <<< whr_

grdUncond :: Where -> Guarded
grdUncond where_ = Guarded $ CST.Unconditional <$> runWhere where_

-- todo grdGuarded

-- do statement

newtype DoStatement =
  DoStatement (ModuleBuilder (Maybe CST.DoStatement))

runDoStatement :: DoStatement -> ModuleBuilder (Maybe CST.DoStatement)
runDoStatement (DoStatement mb) = mb

doLet :: Array LetBinding -> DoStatement
doLet letBindings' = DoStatement case NonEmptyArray.fromArray letBindings' of
  Just letBindings ->
    Just <<< CST.DoLet <$> traverse runLetBinding letBindings
  Nothing ->
    pure Nothing

doDiscard :: Expr -> DoStatement
doDiscard =
  DoStatement <<< map (Just <<< CST.DoDiscard) <<< runExpr

doBind :: Binder -> Expr -> DoStatement
doBind binder' expr' = DoStatement ado
  binder <- runBinder binder'
  expr <- runExpr expr'
  in Just $ CST.DoBind { binder, expr }

-- value binding fieldoStatements

valueBindingFields :: String -> Array Binder -> Guarded -> ModuleBuilder CST.ValueBindingFields
valueBindingFields name' binders' guarded' = ado
  name <- mkName name'
  binders <- traverse runBinder binders'
  guarded <- runGuarded guarded'
  in { name, binders, guarded }
