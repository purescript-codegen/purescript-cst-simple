module CST.Simple.ProjectBuilderSpec
       ( projectBuilderSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Names (ModuleName)
import CST.Simple.ProjectBuilder (ProjectBuilder, addModule, buildProject)
import CST.Simple.TestUtils (fooBarModuleName)
import CST.Simple.Types (Project, ModuleEntry)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, error)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)

projectBuilderSpec :: Spec Unit
projectBuilderSpec = describe "ProjectBuilder" do
  moduleNameSpec

moduleNameSpec :: Spec Unit
moduleNameSpec = do
  it "should encode module name" do
    modules <- _.modules <$> buildProject' (addModule "Foo.Bar" (pure unit))
    (getModuleName <$> modules) `shouldEqual` [ fooBarModuleName ]

  it "should reject invalid module name" do
    buildProjectErr (addModule "Foo!" (pure unit))
      `shouldContain` (InvalidModuleName "Foo!")

  it "should reject duplicate module names" do
    buildProjectErr (addModule "Foo" (pure unit) *> addModule "Foo" (pure unit))
      `shouldContain` (DuplicateModuleName "Foo")


getModuleName :: ModuleEntry -> ModuleName
getModuleName { cstModule: CST.Module { moduleName } } = moduleName


buildProject' ::
  forall m.
  MonadThrow Error m =>
  ProjectBuilder Unit ->
  m Project
buildProject' pb = case buildProject pb of
  Left e -> throwError $ error $ "codegen error - " <> show e
  Right r -> pure r

buildProjectErr ::
  ProjectBuilder Unit ->
  Maybe CodegenError
buildProjectErr pb =
  either Just (const Nothing) (buildProject pb)
