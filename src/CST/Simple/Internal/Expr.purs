module CST.Simple.Internal.Expr
       ( Expr
       , runExpr
       , exprIdent
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
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualName)
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
exprConsN c args =
  Expr $ foldl appExpr (runExpr $ exprCons c) args
  where
    appExpr a bExpr =
      CST.ExprApp <$> a <*> runExpr bExpr

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
