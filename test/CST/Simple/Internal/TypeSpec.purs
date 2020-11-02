module CST.Simple.Internal.TypeSpec
       ( typeSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.Type (class AsType, Type, cnst, runType', typApp, typCons, typForall, typOp, typRecord, typRow, typString, typVar, (*->), (*::), (*=>))
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
    "Foo.Bar.Baz" `shouldMatchType`
      ( CST.TypeConstructor $ CST.QualifiedName
        { qualModule: Nothing
        , qualName: CST.ProperName "Baz"
        }
      )

  it "should add qualified names to imports" do
    "Foo.Bar.Baz" `shouldImportType`
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
    appType <- evalTyp $ typApp "Foo.Bar.Baz" ([] :: Array Type)
    consType <- evalTyp $ typCons "Foo.Bar.Baz"
    appType `shouldEqual` consType

  it "should create nested typApp" do
    typApp "Foo" [ "Int", "String", "Boolean" ] `shouldMatchType`
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
    appType <- evalTyp $ typForall [] "Foo.Bar.Baz"
    consType <- evalTyp $ typCons "Foo.Bar.Baz"
    appType `shouldEqual` consType

  it "should treat create forall typ" do
    typForall [ "a", "b", "c" ] "Int" `shouldMatchType`
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
    typForall [ "A" ] "Int" `shouldErrorType`
      (InvalidIdent "A")

  it "should create type arrows" do
    ("Int" *-> "String" *-> "Int") `shouldMatchType`
      (intCSTType `CST.TypeArr` (stringCSTType `CST.TypeArr` intCSTType))

  it "should create kinded types" do
    ("Qux" *:: "Foo.Bar.Baz") `shouldMatchType`
      (CST.TypeKinded (cstTypCons "Qux") (CST.KindName (cstUnqualProperName "Baz")))

  it "should import kinds" do
    ("Qux" *:: "Foo.Bar.Baz") `shouldImportType`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportKind (CST.ProperName "Baz")
               ]
      , qualification: Nothing
      }

  it "should create type operators" do
    (typOp "String" "Foo.Bar.Baz.(><)" "Int") `shouldMatchType`
      (CST.TypeOp
       (cstTypCons "String")
       (cstUnqualName (CST.OpName "><"))
       (cstTypCons "Int")
      )

  it "should import type operators" do
    (typOp "String" "Foo.Bar.(><)" "Int") `shouldImportType`
      CST.ImportDecl
      { moduleName: fooBarModuleName
      , names: [ CST.ImportTypeOp (CST.OpName "><")
               ]
      , qualification: Nothing
      }

  it "should guard against invalid operator" do
    (typOp "String" "Foo.Bar.Baz.(Qux)" "Int")  `shouldErrorType`
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

evalTyp :: forall t m. MonadThrow Error m => AsType t => t -> m CST.Type
evalTyp t = do
  buildA (runType' t)

shouldMatchType :: forall t m. MonadThrow Error m => AsType t => t -> CST.Type -> m Unit
shouldMatchType t cstType = do
  evalTyp t `shouldReturn` cstType

shouldErrorType :: forall t m. MonadThrow Error m => AsType t => t -> CodegenError -> m Unit
shouldErrorType t err =
   buildModuleErr (runType' t) `shouldReturn` err

shouldImportType :: forall t m. MonadThrow Error m => AsType t => t -> CST.ImportDecl -> m Unit
shouldImportType t import_ =
  shouldImport (runType' t) import_
