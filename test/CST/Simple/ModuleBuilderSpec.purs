module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.ModuleBuilder (addTypeDecl)
import CST.Simple.TestUtils (build, buildModuleErr, intCSTType, requireOne)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldReturn)

moduleBuilderSpec :: Spec Unit
moduleBuilderSpec = describe "ModuleBuilder" do
  typeDeclarationSpec

typeDeclarationSpec :: Spec Unit
typeDeclarationSpec = do
  it "should reject type declarations with invalid name" do
    buildModuleErr (addTypeDecl "x" "Int") `shouldReturn` (InvalidDataHeadName "x" (InvalidTypeName "x"))

  it "should reject duplicate declarations" do
    buildModuleErr (addTypeDecl "X" "Int" *> addTypeDecl "X" "String")
      `shouldReturn` (DuplicateDeclName "X")

  it "should accept type declarations" do
    mod <- build (addTypeDecl "X" "Int")
    mod.declarations `shouldContain`
      CST.DeclType
        { comments: Nothing
        , head: CST.DataHead
          { dataHdName: CST.ProperName "X"
          , dataHdVars: []
          }
        , type_: intCSTType
        }

  it "should not duplicate imports" do
    mod <- build (addTypeDecl "X" "Foo.Bar.Baz" *> addTypeDecl "Y" "Foo.Bar.Baz")
    CST.ImportDecl { names } <- requireOne mod.imports
    Array.length names `shouldEqual` 1
