module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import CST.Simple.ModuleBuilder (ModuleBuilder, addTypeDecl, buildModule, typString)
import CST.Simple.TestUtils (fooBarModuleName)
import CST.Simple.Types (CodegenError(..), ModuleContent)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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
        , type_: CST.TypeConstructor $ CST.QualifiedName
          { qualModule: Nothing
          , qualName: CST.ProperName "Int"
          }
        }

  it "should accept qualified type declarations" do
    mod <- buildModule' (addTypeDecl "X" "Foo.Bar.Baz")
    decl <- requireOne mod.declarations
    type_ <- decl `requireMatch` case _ of
      CST.DeclType { type_ } -> Just type_
      _ -> Nothing
    type_ `shouldEqual`
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

  it "should add type symbol declarations" do
    mod <- buildModule' (addTypeDecl "X" (typString "foo"))
    decl <- requireOne mod.declarations
    decl `requireMatch` case _ of
      CST.DeclType { type_: CST.TypeString "foo" } -> Just unit
      _ -> Nothing


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
