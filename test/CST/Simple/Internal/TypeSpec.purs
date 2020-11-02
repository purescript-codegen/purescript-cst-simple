module CST.Simple.Internal.TypeSpec
       ( typeSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.CommonOp ((*->), (*::), (*=>))
import CST.Simple.Internal.Type (Type, cnst, runType, typ, typApp, typCons, typForall, typOp, typRecord, typRow, typString, typVar)
import CST.Simple.TestUtils (buildA, buildModuleErr, cstTypCons, cstUnqualName, cstUnqualProperName, fooBarModuleName, intCSTType, shouldImport, stringCSTType)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)

typeSpec :: Spec Unit
typeSpec = describe "Type" do
  it "should accept qualified type declarations" do
    typ "Foo.Bar.Baz" `shouldMatchType`
      ( CST.TypeConstructor $ CST.QualifiedName
        { qualModule: Nothing
        , qualName: CST.ProperName "Baz"
        }
      )

  it "should add qualified names to imports" do
    typ "Foo.Bar.Baz" `shouldImportType`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportType (CST.ProperName "Baz") Nothing -- todo import data type
               ]
      , qualification: Nothing
      }

  it "should add type var" do
    typVar "x" `shouldMatchType` CST.TypeVar (CST.Ident "x")

  it "should reject invalid type var" do
    typVar "X" `shouldErrorType` InvalidIdent "X"

  it "should add type symbol declarations" do
    typString "foo" `shouldMatchType` CST.TypeString "foo"

  it "should add type row" do
    (typRow [ "a" /\ typCons "Int", "B" /\ typVar "x" ] (Just "y"))
      `shouldMatchType`
      CST.TypeRow
      { rowLabels:
        [ { label: CST.Label "a", type_: intCSTType }
        , { label: CST.Label "B", type_: CST.TypeVar (CST.Ident "x") }
        ]
      , rowTail:
        Just $ CST.TypeVar (CST.Ident "y")
      }

  it "should reject invalid row tail" do
    typRow [] (Just "Z") `shouldErrorType` InvalidIdent "Z"

  it "should add type record" do
    (typRecord [ "a" /\ typCons "Int", "B" /\ typVar "x" ] (Just "y"))
      `shouldMatchType`
      CST.TypeRecord
      { rowLabels:
        [ { label: CST.Label "a", type_: intCSTType }
        , { label: CST.Label "B", type_: CST.TypeVar (CST.Ident "x") }
        ]
      , rowTail:
        Just $ CST.TypeVar (CST.Ident "y")
      }

  it "should reject invalid record tail" do
    typRecord [] (Just "Z") `shouldErrorType` InvalidIdent "Z"

  it "should treat empty typApp as typCons" do
    appType <- evalTyp $ typApp (typ "Foo.Bar.Baz") []
    consType <- evalTyp $ typCons "Foo.Bar.Baz"
    appType `shouldEqual` consType

  it "should create nested typApp" do
    typApp (typ "Foo") [ typ "Int", typ "String", typ "Boolean" ] `shouldMatchType`
      CST.TypeApp
      ( CST.TypeApp
        ( CST.TypeApp
          (cstTypCons "Foo")
          (cstTypCons "Int")
        )
        (cstTypCons "String")
      )
      (cstTypCons "Boolean")

  it "should treat empty forall as typCons" do
    appType <- evalTyp $ typForall [] (typ "Foo.Bar.Baz")
    consType <- evalTyp $ typCons "Foo.Bar.Baz"
    appType `shouldEqual` consType

  it "should treat create forall typ" do
    typForall [ "a", "b", "c" ] (typ "Int") `shouldMatchType`
      ( CST.TypeForall
        ( NonEmptyArray.cons'
          (CST.TypeVarName (CST.Ident "a"))
          [ CST.TypeVarName (CST.Ident "b")
          , CST.TypeVarName (CST.Ident "c")
          ]
        )
        intCSTType
      )

  it "should catch errors in type vars" do
    typForall [ "A" ] (typ "Int") `shouldErrorType`
      (InvalidIdent "A")

  it "should create type arrows" do
    (typ "Int" *-> typ "String" *-> typ "Int") `shouldMatchType`
      (intCSTType `CST.TypeArr` (stringCSTType `CST.TypeArr` intCSTType))

  it "should create kinded types" do
    (typ "Qux" *:: "Foo.Bar.Baz") `shouldMatchType`
      (CST.TypeKinded (cstTypCons "Qux") (CST.KindName (cstUnqualProperName "Baz")))

  it "should import kinds" do
    (typ "Qux" *:: "Foo.Bar.Baz") `shouldImportType`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportKind (CST.ProperName "Baz")
               ]
      , qualification: Nothing
      }

  it "should create type operators" do
    (typOp (typ "String") "Foo.Bar.Baz.(><)" (typ "Int")) `shouldMatchType`
      (CST.TypeOp
       (cstTypCons "String")
       (cstUnqualName (CST.OpName "><"))
       (cstTypCons "Int")
      )

  it "should import type operators" do
    (typOp (typ "String") "Foo.Bar.(><)" (typ "Int")) `shouldImportType`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportTypeOp (CST.OpName "><")
               ]
      , qualification: Nothing
      }

  it "should guard against invalid operator" do
    (typOp (typ "String") "Foo.Bar.Baz.(Qux)" (typ "Int"))  `shouldErrorType`
      (InvalidQualifiedName "Foo.Bar.Baz.(Qux)" "(Qux)" (Just (InvalidTypeOpName "Qux")))

  it "should create constrained types" do
    (cnst "Foo.Bar.Baz" [ typVar "a" ] *=> typVar "a") `shouldMatchType`
      CST.TypeConstrained
      ( CST.Constraint
        { className: cstUnqualProperName "Baz"
        , args: [ CST.TypeVar (CST.Ident "a")
                ]
        }
      )
      ( CST.TypeVar (CST.Ident "a")
      )

  it "should import class constraints constrained types" do
    (cnst "Foo.Bar.Baz" [ typVar "a" ] *=> typVar "a") `shouldImportType`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names:
        [ CST.ImportClass (CST.ProperName "Baz")
        ]
      , qualification: Nothing
      }

evalTyp :: forall m. MonadThrow Error m => Type -> m CST.Type
evalTyp t = do
  buildA (runType t)

shouldMatchType :: forall m. MonadThrow Error m => Type -> CST.Type -> m Unit
shouldMatchType t cstType = do
  evalTyp t `shouldReturn` cstType

shouldErrorType :: forall m. MonadThrow Error m => Type -> CodegenError -> m Unit
shouldErrorType t err =
   buildModuleErr (runType t) `shouldReturn` err

shouldImportType :: forall m. MonadThrow Error m => Type -> CST.ImportDecl -> m Unit
shouldImportType t import_ =
  shouldImport (runType t) import_
