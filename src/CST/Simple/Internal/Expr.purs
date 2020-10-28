module CST.Simple.Internal.Expr
       ( Expr
       , runExpr
       , exprIdent
       , exprString
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkQualCSTIdent)
import Language.PS.CST as CST

newtype Expr = Expr (ModuleBuilder CST.Expr)

runExpr :: forall m. Monad m => Expr -> ModuleBuilderT m CST.Expr
runExpr (Expr mb) = liftModuleBuilder mb

exprIdent :: String -> Expr
exprIdent s = Expr do
  qi <- mkQualCSTIdent s
  pure $ CST.ExprIdent qi

exprString :: String -> Expr
exprString = Expr <<< pure <<< CST.ExprString