module CST.Simple.Internal.ExprSpec
       ( exprSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError)
import CST.Simple.Internal.Expr (Expr, exprCons, exprIdent, exprString, runExpr)
import CST.Simple.TestUtils (buildA, buildModuleErr, cstUnqualIdent, cstUnqualProperName, fooBarModuleName, shouldImport)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
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
      CST.ExprIdent (cstUnqualIdent "baz")

  it "should import qualified ident" do
    exprIdent "Foo.Bar.baz" `shouldImportExpr`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportValue (CST.Ident "baz")
               ]
      , qualification: Nothing
      }

  it "should create unqualified constructor" do
    exprCons "BazA" [] `shouldMatchExpr`
      CST.ExprConstructor (cstUnqualProperName "BazA")

  it "should create qualified constructor" do
    exprCons "Foo.Bar.Baz(BazA)" [] `shouldMatchExpr`
      CST.ExprConstructor (cstUnqualProperName "BazA")

  it "should import qualified ident" do
    exprIdent "Foo.Bar.baz" `shouldImportExpr`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportValue (CST.Ident "baz")
               ]
      , qualification: Nothing
      }

  it "should create string expr" do
    exprString "foo" `shouldMatchExpr`
      CST.ExprString "foo"

shouldMatchExpr :: forall m. MonadThrow Error m => Expr -> CST.Expr -> m Unit
shouldMatchExpr e cstExpr = do
  buildA (runExpr e) `shouldReturn` cstExpr

shouldErrorExpr :: forall m. MonadThrow Error m => Expr -> CodegenError -> m Unit
shouldErrorExpr e err =
   buildModuleErr (runExpr e) `shouldReturn` err

shouldImportExpr :: forall m. MonadThrow Error m => Expr-> CST.ImportDecl -> m Unit
shouldImportExpr t import_ =
  shouldImport (runExpr t) import_
