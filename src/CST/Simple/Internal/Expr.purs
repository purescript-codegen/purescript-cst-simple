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
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualName)
import CST.Simple.Internal.RecordLabeled (RecordLabeled, runRecordLabeled)
import CST.Simple.Names (TypedConstructorName(..))
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
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
