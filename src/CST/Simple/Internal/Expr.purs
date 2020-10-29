module CST.Simple.Internal.Expr
       ( Expr
       , runExpr
       , exprIdent
       , exprCons
       , exprString
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName, mkQualName)
import CST.Simple.Names (TypedConstructorName(..))
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST

newtype Expr = Expr (ModuleBuilder CST.Expr)

runExpr :: forall m. Monad m => Expr -> ModuleBuilderT m CST.Expr
runExpr (Expr mb) = liftModuleBuilder mb

exprIdent :: String -> Expr
exprIdent s = Expr $ CST.ExprIdent <$> mkQualName s

exprCons :: String -> Array String -> Expr
exprCons c _ = Expr $ CST.ExprConstructor <$> (qualifiedCons <|> unqualifiedCons)
  where
    qualifiedCons =
      map getNamePart <$> mkQualName c

    unqualifiedCons = mkName c <#> \name ->
      CST.QualifiedName { qualModule: Nothing
                        , qualName: name
                        }

    getNamePart (TypedConstructorName _ n) = n

exprString :: String -> Expr
exprString = Expr <<< pure <<< CST.ExprString
