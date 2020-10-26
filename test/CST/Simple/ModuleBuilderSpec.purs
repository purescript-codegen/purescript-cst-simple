module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import CST.Simple.ModuleBuilder (class AsTyp, ModuleBuilder, Typ, addTypeDecl, buildModule, typApp, typCons, typForall, typRecord, typRow, typString, typVar, (*->), (*::))
import CST.Simple.TestUtils (fooBarModuleName)
import CST.Simple.Types (CodegenError(..), ModuleContent)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error, error)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldReturn)

moduleBuilderSpec :: Spec Unit
moduleBuilderSpec = describe "ModuleBuilder" do
  declarationSpec

declarationSpec :: Spec Unit
declarationSpec = do
  it "should reject type declarations with invalid name" do
    buildModuleErr (addTypeDecl "x" "Int") `shouldReturn` (InvalidProperName "x")

  it "should reject type declarations with invalid value" do
    buildModuleErr (addTypeDecl "X" "int") `shouldReturn` (InvalidQualifiedName "int")

  it "should reject duplicate declarations" do
    buildModuleErr (addTypeDecl "X" "Int" *> addTypeDecl "X" "String")
      `shouldReturn` (DuplicateDeclName "X")

  it "should accept type declarations" do
    mod <- buildModule' (addTypeDecl "X" "Int")
    mod.declarations `shouldContain`
      CST.DeclType
        { comments: Nothing
        , head: CST.DataHead
          { dataHdName: CST.ProperName "X"
          , dataHdVars: []
          }
        , type_: intCSTType
        }

  it "should accept qualified type declarations" do
    "Foo.Bar.Baz" `shouldMatchType`
      ( CST.TypeConstructor $ CST.QualifiedName
        { qualModule: Nothing
        , qualName: CST.ProperName "Baz"
        }
      )

  it "should add qualified names to imports" do
    mod <- buildModule' (addTypeDecl "X" "Foo.Bar.Baz")
    mod.imports `shouldContain`
      ( CST.ImportDecl
        { moduleName: fooBarModuleName
        , names: [ CST.ImportType (CST.ProperName "Baz") Nothing -- todo import data type
                 ]
        , qualification: Nothing
        }
      )

  it "should not duplicate imports" do
    mod <- buildModule' (addTypeDecl "X" "Foo.Bar.Baz" *> addTypeDecl "Y" "Foo.Bar.Baz")
    CST.ImportDecl { names } <- requireOne mod.imports
    Array.length names `shouldEqual` 1

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
    appType <- evalTyp $ typApp "Foo.Bar.Baz" ([] :: Array Typ)
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
      (CST.TypeKinded (cstTypCons "Qux") (CST.KindName (cstUnqualName "Baz")))

  it "should import kinds" do
    mod <- buildModule' (addTypeDecl "X" ("Qux" *:: "Foo.Bar.Baz"))
    mod.imports `shouldContain`
      ( CST.ImportDecl
        { moduleName: fooBarModuleName
        , names: [ CST.ImportKind (CST.ProperName "Baz")
                 ]
        , qualification: Nothing
        }
      )

buildModule' :: forall m. MonadThrow Error m => ModuleBuilder Unit -> m ModuleContent
buildModule' mb = case buildModule mb of
  Left e ->
    throwError $ error $ "codegen error - " <> show e
  Right r ->
    pure r

buildModuleErr :: forall m. MonadThrow Error m => ModuleBuilder Unit -> m CodegenError
buildModuleErr mb = case buildModule mb of
  Left e ->
    pure e
  Right r ->
    throwError $ error $ "failed to get a codegen error when one was expected"

requireOne :: forall m a. MonadThrow Error m => Array a -> m a
requireOne [a] = pure a
requireOne as = throwError $ error $ "expected 1 entry. got " <> show (Array.length as) <> " entries"

requireMatch :: forall m a b. Show a => MonadThrow Error m => a -> (a -> Maybe b) -> m b
requireMatch a f = case f a of
  Just b -> pure b
  Nothing -> throwError $ error $ "failed to match - "  <> show a

evalTyp :: forall t m. MonadThrow Error m => AsTyp t => t -> m CST.Type
evalTyp t = do
  mod <- buildModule' (addTypeDecl "X" t)
  decl <- requireOne mod.declarations
  decl `requireMatch` case _ of
    CST.DeclType { type_ } -> Just type_
    _ -> Nothing

shouldMatchType :: forall t m. MonadThrow Error m => AsTyp t => t -> CST.Type -> m Unit
shouldMatchType t cstType = do
  evalTyp t `shouldReturn` cstType

shouldErrorType :: forall t m. MonadThrow Error m => AsTyp t => t -> CodegenError -> m Unit
shouldErrorType t err =
   buildModuleErr (addTypeDecl "X" t) `shouldReturn` err

intCSTType :: CST.Type
intCSTType =
  CST.TypeConstructor $ CST.QualifiedName
  { qualModule: Nothing
  , qualName: CST.ProperName "Int"
  }

stringCSTType :: CST.Type
stringCSTType =
  CST.TypeConstructor $ CST.QualifiedName
  { qualModule: Nothing
  , qualName: CST.ProperName "String"
  }

cstTypCons :: String -> CST.Type
cstTypCons = CST.TypeConstructor <<< cstUnqualName

cstUnqualName :: forall p. String -> CST.QualifiedName (CST.ProperName p)
cstUnqualName n =
  CST.QualifiedName
  { qualModule: Nothing
  , qualName: CST.ProperName n
  }
