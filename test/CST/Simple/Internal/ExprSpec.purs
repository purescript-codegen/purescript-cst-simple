module CST.Simple.Internal.ExprSpec
       ( exprSpec
       ) where

import Prelude

import CST.Simple.Internal.Expr (Expr, exprIdent, exprString, runExpr)
import CST.Simple.TestUtils (buildA, buildModuleErr, cstUnqualIdent)
import CST.Simple.Types (CodegenError)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldReturn)

exprSpec :: Spec Unit
exprSpec = describe "Expr" do
  it "should create unqualified ident" do
    exprIdent "baz" `shouldMatchExpr`
      CST.ExprIdent (cstUnqualIdent "baz")

  it "should create qualified ident" do
    exprIdent "Foo.Bar.baz" `shouldMatchExpr`
      CST.ExprIdent (cstUnqualIdent "foo")

{-
  it "should import qualified ident" do
    exprIdent "Foo.Bar.baz" `shouldImport`
      CST.ExprIdent $ cstUnqualified "foo"
-}

  it "should create string expr" do
    exprString "foo" `shouldMatchExpr`
      CST.ExprString "foo"

shouldMatchExpr :: forall m. MonadThrow Error m => Expr -> CST.Expr -> m Unit
shouldMatchExpr e cstExpr = do
  buildA (runExpr e) `shouldReturn` cstExpr

shouldErrorExpr :: forall m. MonadThrow Error m => Expr -> CodegenError -> m Unit
shouldErrorExpr e err =
   buildModuleErr (runExpr e) `shouldReturn` err
