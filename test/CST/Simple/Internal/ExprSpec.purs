module CST.Simple.Internal.ExprSpec
       ( exprSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError)
import CST.Simple.Internal.Expr (Expr, exprArray, exprBoolean, exprChar, exprCons, exprConsN, exprIdent, exprInt, exprNumber, exprString, runExpr)
import CST.Simple.TestUtils (buildA, buildModuleErr, cstUnqualIdent, cstUnqualProperName, fooBarModuleName, shouldImport)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
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

  it "should import qualified ident" do
    exprIdent "Foo.Bar.baz" `shouldImportExpr`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportValue (CST.Ident "baz")
               ]
      , qualification: Nothing
      }

  it "should create unqualified constructor" do
    exprCons "BazA" `shouldMatchExpr`
      CST.ExprConstructor (cstUnqualProperName "BazA")

  it "should create qualified constructor" do
    exprCons "Foo.Bar.Baz(BazA)" `shouldMatchExpr`
      CST.ExprConstructor (cstUnqualProperName "BazA")

  it "should create constructor with args" do
    exprConsN "BazA"
      [ exprIdent "a"
      , exprIdent "b"
      ]
      `shouldMatchExpr`
      CST.ExprApp
      (CST.ExprApp
        (CST.ExprConstructor (cstUnqualProperName "BazA"))
        (CST.ExprIdent (cstUnqualIdent "a"))
      )
      (CST.ExprIdent (cstUnqualIdent "b"))

  it "should create boolean expr" do
    exprBoolean true `shouldMatchExpr`
      CST.ExprBoolean true

  it "should create char expr" do
    exprChar 'x' `shouldMatchExpr`
      CST.ExprChar 'x'

  it "should create string expr" do
    exprString "foo" `shouldMatchExpr`
      CST.ExprString "foo"

  it "should create int expr" do
    exprInt 5 `shouldMatchExpr`
      CST.ExprNumber (Left 5)

  it "should create number expr" do
    exprNumber 5.0 `shouldMatchExpr`
      CST.ExprNumber (Right 5.0)

  it "should create array expr" do
    exprArray [ exprNumber 5.0 ] `shouldMatchExpr`
      CST.ExprArray
      [ CST.ExprNumber (Right 5.0)
      ]

shouldMatchExpr :: forall m. MonadThrow Error m => Expr -> CST.Expr -> m Unit
shouldMatchExpr e cstExpr = do
  buildA (runExpr e) `shouldReturn` cstExpr

shouldErrorExpr :: forall m. MonadThrow Error m => Expr -> CodegenError -> m Unit
shouldErrorExpr e err =
   buildModuleErr (runExpr e) `shouldReturn` err

shouldImportExpr :: forall m. MonadThrow Error m => Expr-> CST.ImportDecl -> m Unit
shouldImportExpr t import_ =
  shouldImport (runExpr t) import_
