module CST.Simple.Internal.Expr
       ( Expr
       , runExpr
       , exprIdent
       , exprCons
       , exprCons0
       , exprCons1
       , exprCons2
       , exprCons3
       , exprCons4
       , exprCons5
       , exprCons6
       , exprString
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualName)
import CST.Simple.Names (TypedConstructorName(..))
import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST

newtype Expr = Expr (ModuleBuilder CST.Expr)

runExpr :: forall m. Monad m => Expr -> ModuleBuilderT m CST.Expr
runExpr (Expr mb) = liftModuleBuilder mb

exprIdent :: String -> Expr
exprIdent s = Expr $ CST.ExprIdent <$> mkQualName s

exprCons :: String -> Array Expr -> Expr
exprCons c args =
  Expr $ foldl appExpr (runExpr $ exprCons0 c) args
  where
    appExpr a bExpr =
      CST.ExprApp <$> a <*> runExpr bExpr

exprCons0 :: String -> Expr
exprCons0 c = Expr $ CST.ExprConstructor <$> (qualifiedCons <|> unqualifiedCons)
  where
    qualifiedCons =
      map getNamePart <$> mkQualName c

    unqualifiedCons = mkName c <#> \name ->
      CST.QualifiedName { qualModule: Nothing
                        , qualName: name
                        }

    getNamePart (TypedConstructorName _ n) = n

exprCons1 :: String -> Expr -> Expr
exprCons1 c a1 = exprCons c [ a1 ]

exprCons2 :: String -> Expr -> Expr -> Expr
exprCons2 c a1 a2 = exprCons c [ a1, a2 ]

exprCons3 :: String -> Expr -> Expr -> Expr -> Expr
exprCons3 c a1 a2 a3 = exprCons c [ a1, a2, a3 ]

exprCons4 :: String -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons4 c a1 a2 a3 a4 = exprCons c [ a1, a2, a3, a4 ]

exprCons5 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons5 c a1 a2 a3 a4 a5 = exprCons c [ a1, a2, a3, a4, a5 ]

exprCons6 :: String -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
exprCons6 c a1 a2 a3 a4 a5 a6 = exprCons c [ a1, a2, a3, a4, a5, a6 ]

exprString :: String -> Expr
exprString = Expr <<< pure <<< CST.ExprString
