module CST.Simple.Internal.ExprSpec
       ( exprSpec
       ) where

import Prelude

import CST.Simple.Internal.Binder (bndrVar)
import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.CommonOp ((*->))
import CST.Simple.Internal.Expr (Expr, exprArray, exprBoolean, exprChar, exprCons, exprConsN, exprIdent, exprIdentN, exprIfThenElse, exprInt, exprNegate, exprNumber, exprOp, exprOpName, exprRecord, exprRecordAccess, exprRecordAccessN, exprRecordUpdate, exprString, exprTyped, recordUpdate, recordUpdateBranch, runExpr)
import CST.Simple.Internal.RecordLabeled (recField, recPun)
import CST.Simple.Internal.Type (typCons)
import CST.Simple.TestUtils (build', buildA, buildModuleErr, cstUnqualIdent, cstUnqualOpName, cstUnqualProperName, fooBarModuleName, shouldImport)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)

exprSpec :: Spec Unit
exprSpec = describe "Expr" do
  it "should create unqualified ident" do
    exprIdent "baz" `shouldMatchCSTExpr`
      CST.ExprIdent (cstUnqualIdent "baz")

  it "should create qualified ident" do
    exprIdent "Foo.Bar.baz" `shouldMatchCSTExpr`
      CST.ExprIdent (cstUnqualIdent "baz")

  it "should import qualified ident" do
    exprIdent "Foo.Bar.baz" `exprShouldImport`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportValue (CST.Ident "baz")
               ]
      , qualification: Nothing
      }

  it "should import qualified ident" do
    exprIdent "Foo.Bar.baz" `exprShouldImport`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportValue (CST.Ident "baz")
               ]
      , qualification: Nothing
      }

  it "should create ident with args" do
    exprIdentN "foo"
      [ exprIdent "a"
      , exprIdent "b"
      ]
      `shouldMatchCSTExpr`
      CST.ExprApp
      (CST.ExprApp
        (CST.ExprIdent (cstUnqualIdent "foo"))
        (CST.ExprIdent (cstUnqualIdent "a"))
      )
      (CST.ExprIdent (cstUnqualIdent "b"))


  it "should create unqualified constructor" do
    exprCons "BazA" `shouldMatchCSTExpr`
      CST.ExprConstructor (cstUnqualProperName "BazA")

  it "should create qualified constructor" do
    exprCons "Foo.Bar.Baz(BazA)" `shouldMatchCSTExpr`
      CST.ExprConstructor (cstUnqualProperName "BazA")

  it "should create constructor with args" do
    exprConsN "BazA"
      [ exprIdent "a"
      , exprIdent "b"
      ]
      `shouldMatchCSTExpr`
      CST.ExprApp
      (CST.ExprApp
        (CST.ExprConstructor (cstUnqualProperName "BazA"))
        (CST.ExprIdent (cstUnqualIdent "a"))
      )
      (CST.ExprIdent (cstUnqualIdent "b"))

  it "should create boolean expr" do
    exprBoolean true `shouldMatchCSTExpr`
      CST.ExprBoolean true

  it "should create char expr" do
    exprChar 'x' `shouldMatchCSTExpr`
      CST.ExprChar 'x'

  it "should create string expr" do
    exprString "foo" `shouldMatchCSTExpr`
      CST.ExprString "foo"

  it "should create int expr" do
    exprInt 5 `shouldMatchCSTExpr`
      CST.ExprNumber (Left 5)

  it "should create number expr" do
    exprNumber 5.0 `shouldMatchCSTExpr`
      CST.ExprNumber (Right 5.0)

  it "should create array expr" do
    exprArray [ exprNumber 5.0 ] `shouldMatchCSTExpr`
      CST.ExprArray
      [ CST.ExprNumber (Right 5.0)
      ]

  it "should create record expr" do
    exprRecord [ recField "foo" (exprInt 1)
               , recField "bar" (exprInt 2)
               , recPun "baz"
               ]
      `shouldMatchCSTExpr`
      CST.ExprRecord
      [ CST.RecordField (CST.Label "foo") (CST.ExprNumber (Left 1))
      , CST.RecordField (CST.Label "bar") (CST.ExprNumber (Left 2))
      , CST.RecordPun (CST.Ident "baz")
      ]

  it "should reject invalid record puns" do
    exprRecord [ recPun "!"
               ]
      `exprShouldError`
      InvalidIdent "!"

  it "should create typed expr" do
    exprTyped (exprInt 5) (typCons "Int")
      `shouldMatchCSTExpr`
      CST.ExprTyped
      (CST.ExprNumber (Left 5))
      (CST.TypeConstructor (cstUnqualProperName "Int"))

  it "should create expr with operation" do
    (exprOp (exprInt 5) "Prelude.(+)" (exprInt 5))
      `shouldMatchCSTExpr`
      CST.ExprOp
      (CST.ExprNumber (Left 5))
      (cstUnqualOpName "+")
      (CST.ExprNumber (Left 5))

  it "should create exprOpName" do
    (exprOpName "Prelude.(+)")
      `shouldMatchCSTExpr`
      CST.ExprOpName
      (cstUnqualOpName "+")

  it "should create negative expr" do
    (exprNegate (exprInt 5))
      `shouldMatchCSTExpr`
      CST.ExprNegate
      (CST.ExprNumber (Left 5))

  it "should treat empty record access as identity" do
    (exprRecordAccessN (exprIdent "a") [])
      `shouldBeEquivExpr`
      (exprIdent "a")

  it "should create record accesor" do
    (exprRecordAccessN (exprIdent "a") ["x", "y", "z"])
      `shouldMatchCSTExpr`
      CST.ExprRecordAccessor
      { recExpr: CST.ExprIdent (cstUnqualIdent "a")
      , recPath: CST.Label <$> NonEmptyArray.cons' "x" [ "y", "z" ]
      }

  it "should create record accesor from string" do
    (exprRecordAccess (exprIdent "a") "x.y.z")
      `shouldBeEquivExpr`
      (exprRecordAccessN (exprIdent "a") ["x", "y", "z"])

  it "should create record update" do
    (exprRecordUpdate (exprIdent "a")
     [ recordUpdate "x" (exprIdent "x'")
     , recordUpdateBranch "y"
       [ recordUpdate "z" (exprIdent "z'")
       ]
     ]
    ) `shouldMatchCSTExpr`
      CST.ExprRecordUpdate
      (CST.ExprIdent (cstUnqualIdent "a"))
      ( NonEmptyArray.cons'
        (CST.RecordUpdateLeaf (CST.Label "x") (CST.ExprIdent (cstUnqualIdent "x'")))
        [ CST.RecordUpdateBranch (CST.Label "y")
          (NonEmptyArray.singleton (CST.RecordUpdateLeaf (CST.Label "z") (CST.ExprIdent (cstUnqualIdent "z'"))))
        ]
      )

  it "should ignore empty record update branch" do
    (exprRecordUpdate (exprIdent "a")
     [ recordUpdate "x" (exprIdent "x'")
     , recordUpdateBranch "y" []
     ]
    ) `shouldBeEquivExpr`
      (exprRecordUpdate (exprIdent "a")
       [ recordUpdate "x" (exprIdent "x'")
       ]
      )

  it "should create lambda" do
    ([ bndrVar "x" ] *-> exprIdent "x")
      `shouldMatchCSTExpr`
      CST.ExprLambda
      { binders:
        NonEmptyArray.singleton (CST.BinderVar (CST.Ident "x"))
      , body:
        CST.ExprIdent (cstUnqualIdent "x")
      }

  it "should create if then else" do
    (exprIfThenElse (exprIdent "x") (exprIdent "y") (exprIdent "z")) `shouldMatchCSTExpr`
      CST.ExprIf
      { cond: CST.ExprIdent (cstUnqualIdent "x")
      , true_: CST.ExprIdent (cstUnqualIdent "y")
      , false_: CST.ExprIdent (cstUnqualIdent "z")
      }

shouldMatchCSTExpr :: forall m. MonadThrow Error m => Expr -> CST.Expr -> m Unit
shouldMatchCSTExpr e cstExpr = do
  buildA (runExpr e) `shouldReturn` cstExpr

shouldBeEquivExpr :: forall m. MonadThrow Error m => Expr -> Expr -> m Unit
shouldBeEquivExpr e1 e2 = do
  e1' <- build' (runExpr e1)
  e2' <- build' (runExpr e2)
  e1' `shouldEqual` e2'

exprShouldError :: forall m. MonadThrow Error m => Expr -> CodegenError -> m Unit
exprShouldError e err =
   buildModuleErr (runExpr e) `shouldReturn` err

exprShouldImport :: forall m. MonadThrow Error m => Expr-> CST.ImportDecl -> m Unit
exprShouldImport t import_ =
  shouldImport (runExpr t) import_
