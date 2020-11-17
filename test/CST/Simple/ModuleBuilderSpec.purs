module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import CST.Simple.Internal.Binder (bndrVar)
import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.CommonOp ((*->))
import CST.Simple.Internal.Declaration (Declaration, dataCtor, declClass, declData, declForeignValue, declInstance, declInstanceChain, declNewtype, declSignature, declType, declValue, instance_, runDeclaration)
import CST.Simple.Internal.Expression (exprInt, grd_)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, buildModule, exportAll)
import CST.Simple.Internal.Type (cnst, typ, typVar)
import CST.Simple.Internal.TypeVarBinding (tvb)
import CST.Simple.ModuleBuilder (addClassDecl, addDataDecl, addForeignJsValue, addInstanceChainDecl, addInstanceDecl, addNewtypeDecl, addType, addValue)
import CST.Simple.TestUtils (build, build', buildA, requireOne)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Effect.Exception (Error)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)

moduleBuilderSpec :: Spec Unit
moduleBuilderSpec = describe "ModuleBuilder" do
  moduleNameSpec
  importsSpec
  exportsSpec
  declarationsSpec

moduleNameSpec :: Spec Unit
moduleNameSpec = describe "module name" do
  it "should reject invalid module names" do
    buildModule "foo" (pure unit) `shouldEqual` Left (InvalidModuleName "foo")

importsSpec :: Spec Unit
importsSpec = describe "imports" do
  it "should not duplicate imports" do
    mod <- build do
      addType
        { name: "X"
        , typeVarBindings: []
        , type_: typ "Foo.Bar.Baz"
        , export: false
        }
      addType
        { name: "Y"
        , typeVarBindings: []
        , type_: typ "Foo.Bar.Baz"
        , export: false
        }
    CST.ImportDecl { names } <- requireOne mod.imports
    Array.length names `shouldEqual` 1

exportsSpec :: Spec Unit
exportsSpec = describe "exports" do
  it "should disallow empty exports" do
    buildModule "Foo" (pure unit) `shouldEqual` Left MissingExports

  it "should allow export all" do
    mc <- build exportAll
    mc.exports `shouldEqual` []

declarationsSpec :: Spec Unit
declarationsSpec = do
  it "should add values with signature" do
    addValue
      { name: "x"
      , type_: typ "Int" *-> typ "Int"
      , binders: [ bndrVar "a" ]
      , expr: exprInt 5
      , export: true
      }
      `shouldContainDeclarations`
      [ declSignature "x" (typ "Int" *-> typ "Int")
      , declValue "x" [ bndrVar "a" ] (grd_ (exprInt 5))
      ]

  it "should export value" do
    addValue
      { name: "x"
      , type_: typ "Int"
      , binders: []
      , expr: exprInt 5
      , export: true
      }
      `shouldContainExport`
      CST.ExportValue (CST.Ident "x")

  it "should add foreign js decl" do
    addForeignJsValue
      { name: "x"
      , type_: typ "Int"
      , jsExpr: "5"
      , export: true
      }
      `shouldContainDeclaration`
      declForeignValue "x" (typ "Int")

  it "should add foreign js binding" do
    addForeignJsValue
      { name: "x"
      , type_: typ "Int"
      , jsExpr: "5"
      , export: true
      }
      `shouldContainForeign`
      "exports.x = 5;\n"

  it "should export foreign js binding" do
    addForeignJsValue
      { name: "x"
      , type_: typ "Int"
      , jsExpr: "5"
      , export: true
      }
      `shouldContainExport`
      CST.ExportValue (CST.Ident "x")

  it "should add type declarations" do
    addType
      { name: "X"
      , typeVarBindings: []
      , type_: typ "Int"
      , export: false
      }
      `shouldContainDeclaration`
      declType "X" [] (typ "Int")

  it "should export type declaration" do
    addType
      { name: "X"
      , typeVarBindings: []
      , type_: typ "Foo.Bar.Baz"
      , export: true
      }
      `shouldContainExport`
      CST.ExportType (CST.ProperName "X") Nothing

  it "should add data declarations" do
    addDataDecl "Foo" [ tvb "a" ]
      [ dataCtor "Bar" []
      ] `shouldContainDeclaration`
      declData "Foo" [ tvb "a" ]
      [ dataCtor "Bar" []
      ]

  it "should add newtype declarations" do
    addNewtypeDecl "Foo" [ tvb "a" ] "Bar" (typVar "a")
      `shouldContainDeclaration`
      declNewtype "Foo" [ tvb "a" ] "Bar" (typVar "a")

  it "should add class decl" do
    addClassDecl "Foo" [ tvb "a" ] [] [] []
      `shouldContainDeclaration`
      declClass "Foo" [ tvb "a" ] [] [] []

  it "should add single instance" do
    addInstanceDecl "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ] []
      `shouldContainDeclaration`
      declInstance "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ] []

  it "should add instance chain" do
    addInstanceChainDecl
      (instance_ "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ] []
      ) [] `shouldContainDeclaration`
      declInstanceChain
      (instance_ "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ] []
      ) []

shouldContainDeclaration ::
  forall m.
  MonadThrow Error m =>
  ModuleBuilder Unit ->
  Declaration ->
  m Unit
shouldContainDeclaration cmd d =
  shouldContainDeclarations cmd [d]

shouldContainDeclarations ::
  forall m.
  MonadThrow Error m =>
  ModuleBuilder Unit ->
  Array Declaration ->
  m Unit
shouldContainDeclarations cmd ds = do
  mod <- build cmd
  ds' <- traverse (buildA <<< runDeclaration) ds
  mod.declarations `shouldEqual` ds'

shouldContainForeign ::
  forall m.
  MonadThrow Error m =>
  ModuleBuilder Unit ->
  String ->
  m Unit
shouldContainForeign cmd f = do
  mod <- build cmd
  mod.foreignBinding `shouldContain` f

shouldContainExport ::
  forall m.
  MonadThrow Error m =>
  ModuleBuilder Unit ->
  CST.Export ->
  m Unit
shouldContainExport cmd e = do
  mod <- snd <$> build' false cmd
  mod.exports `shouldContain` e
