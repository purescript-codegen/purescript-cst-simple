module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import CST.Simple.Internal.Binder (bndrVar)
import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.CommonOp ((*->))
import CST.Simple.Internal.Declaration (Declaration, dataCtor, declClass, declData, declForeignData, declForeignKind, declForeignValue, declInstance, declInstanceChain, declNewtype, declSignature, declType, declValue, instance_, runDeclaration)
import CST.Simple.Internal.Expression (exprCons, exprIdent, exprInt, grd_)
import CST.Simple.Internal.Kind (knd)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, buildModule, exportAll)
import CST.Simple.Internal.Type (cnst, typ, typApp, typVar)
import CST.Simple.Internal.TypeVarBinding (tvb)
import CST.Simple.ModuleBuilder (addClassDecl, addDataDecl, addForeignData, addForeignJsValue, addInstanceChainDecl, addInstanceDecl, addKind, addNewtype, addType, addValue, reExportClass, reExportKind, reExportOp, reExportType, reExportValue)
import CST.Simple.TestUtils (build, build', buildA, buildModuleErr, fooBarModuleName, getNameError, requireOne)
import CST.Simple.Types (DataExport(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Effect.Exception (Error)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldReturn, shouldSatisfy)

moduleBuilderSpec :: Spec Unit
moduleBuilderSpec = describe "ModuleBuilder" do
  moduleNameSpec
  importsSpec
  exportsSpec
  reExportsSpec
  declarationsSpec

moduleNameSpec :: Spec Unit
moduleNameSpec = describe "module name" do
  it "should reject invalid module names" do
    buildModule "foo" (pure unit) `shouldSatisfy` isLeft

importsSpec :: Spec Unit
importsSpec = describe "imports" do
  it "should not duplicate imports" do
    mod <- build do
      addType
        { name: "X"
        , typeVarBindings: []
        , type_: typApp (typ "Foo") [ typ "Foo.Bar(Baz)", typ "Foo.Bar(Baz)" ]
        , export: false
        }
    CST.ImportDecl { names } <- requireOne mod.imports
    Array.length names `shouldEqual` 1

  it "should join module names" do
    mod <- build do
      addType
        { export: false
        , name: "X"
        , typeVarBindings: []
        , type_: typApp (typ "Foo") [ typ "Foo.Bar(Baz)", typ "Foo.Bar(Qux)" ]
        }
    CST.ImportDecl { names } <- requireOne mod.imports
    Array.length names `shouldEqual` 2

  it "should join module constructor and type" do
    mod <- build do
      addValue
        { export: false
        , name: "x"
        , type_: typ "Foo.Bar(Baz)"
        , binders: []
        , expr: exprCons "Foo.Bar(Baz(Qux))"
        }
    CST.ImportDecl { names } <- requireOne mod.imports
    names `shouldEqual`
      [ CST.ImportType (CST.ProperName "Baz") (Just CST.DataAll)
      ]

  it "should not join aliased module constructor and unaliased type" do
    mod <- build do
      addValue
        { export: false
        , name: "x"
        , type_: typ "Foo.Bar(Baz) as Bar"
        , binders: []
        , expr: exprCons "Foo.Bar(Baz(Qux))"
        }
    Array.length mod.imports `shouldEqual` 2

  it "should import Prelude unqualified" do
    mod <- build do
      addValue
        { export: false
        , name: "x"
        , type_: typ "Foo"
        , binders: []
        , expr: exprIdent "Prelude(bind)"
        }
    CST.ImportDecl { names } <- requireOne mod.imports
    Array.length names `shouldEqual` 0

exportsSpec :: Spec Unit
exportsSpec = describe "exports" do
  it "should disallow empty exports" do
    buildModule "Foo" (pure unit) `shouldEqual` Left MissingExports

  it "should allow export all" do
    mc <- build exportAll
    mc.exports `shouldEqual` []

reExportsSpec :: Spec Unit
reExportsSpec = describe "re-exports" do
  it "should add type re-exports" do
    reExportType "Foo.Bar(Baz)" DataExportType `shouldReExport`
      CST.ImportType (CST.ProperName "Baz") Nothing

    reExportType "Foo.Bar(Baz)" DataExportAll `shouldReExport`
      CST.ImportType (CST.ProperName "Baz") (Just CST.DataAll)

  it "should add value re-exports" do
    reExportValue "Foo.Bar(baz)" `shouldReExport`
      CST.ImportValue (CST.Ident "baz")

  it "should add op re-exports" do
    reExportOp "Foo.Bar((<>))" `shouldReExport`
      CST.ImportOp (CST.OpName "<>")

  it "should add class re-exports" do
    reExportClass "Foo.Bar(class Baz)" `shouldReExport`
      CST.ImportClass (CST.ProperName "Baz")

  it "should add class re-exports" do
    reExportKind "Foo.Bar(kind Baz)" `shouldReExport`
      CST.ImportKind (CST.ProperName "Baz")

  it "should disallow re-export on exportAll" do
    buildModuleErr (exportAll *> reExportValue "Foo.Bar(baz)")
      `shouldReturn` IllegalReExportOnExportAll

  it "should report re-export name error" do
    { allowQualified, allowUnqualified, allowAlias } <-
      getNameError (reExportValue "Foo.Bar(Baz)")
    { allowQualified, allowUnqualified, allowAlias } `shouldEqual`
      { allowQualified: true, allowUnqualified: false, allowAlias: false }

declarationsSpec :: Spec Unit
declarationsSpec = do
  it "should add kind declarations" do
    addKind
      { name: "X"
      , export: false
      }
      `shouldContainDeclaration`
      declForeignKind "X"

  it "should export kind declaration" do
    addKind
      { name: "X"
      , export: true
      }
      `shouldContainExport`
      CST.ExportKind (CST.ProperName "X")

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
      , type_: typ "Foo.Bar(Baz)"
      , export: true
      }
      `shouldContainExport`
      CST.ExportType (CST.ProperName "X") Nothing

  it "should add newtype declarations" do
    addNewtype
      { name: "Foo"
      , export: Nothing
      , typeVarBindings: [ tvb "a" ]
      , consName: "Bar"
      , type_: typVar "a"
      }
      `shouldContainDeclaration`
      declNewtype "Foo" [ tvb "a" ] "Bar" (typVar "a")

  it "should export newtype type only" do
    addNewtype
      { name: "Foo"
      , export: Just DataExportType
      , typeVarBindings: [ tvb "a" ]
      , consName: "Bar"
      , type_: typVar "a"
      }
      `shouldContainExport`
      CST.ExportType (CST.ProperName "Foo") Nothing

  it "should export newtype type and constructor" do
    addNewtype
      { name: "Foo"
      , export: Just DataExportAll
      , typeVarBindings: [ tvb "a" ]
      , consName: "Bar"
      , type_: typVar "a"
      }
      `shouldContainExport`
      CST.ExportType (CST.ProperName "Foo") (Just CST.DataAll)

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

  it "should export foreign js decl" do
    addForeignJsValue
      { name: "x"
      , type_: typ "Int"
      , jsExpr: "5"
      , export: true
      }
      `shouldContainExport`
      CST.ExportValue (CST.Ident "x")

  it "should add foreign data" do
    addForeignData
      { name: "X"
      , kind_: knd "Foo"
      , export: false
      }
      `shouldContainDeclaration`
      declForeignData "X" (knd "Foo")

  it "should export foreign data" do
    addForeignData
      { name: "X"
      , kind_: knd "Foo"
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

shouldReExport ::
  forall m.
  MonadThrow Error m =>
  ModuleBuilder Unit ->
  CST.Import ->
  m Unit
shouldReExport cmd i = do
  mod <- snd <$> build' false cmd
  mod.imports `shouldContain`
    CST.ImportDecl
    { moduleName: fooBarModuleName
    , names: [ i ]
    , qualification: Just $ CST.ModuleName $ NonEmptyArray.singleton $ CST.ProperName "E"
    }
