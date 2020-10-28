module CST.Simple.ProjectBuilderSpec
       ( projectBuilderSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.ProjectBuilder (Project, ProjectBuilder, addModule, buildProject, getCSTModules)
import CST.Simple.TestUtils (fooBarModuleName)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, error)
import Language.PS.CST (Module(..), ModuleName)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)

projectBuilderSpec :: Spec Unit
projectBuilderSpec = describe "ProjectBuilder" do
  moduleNameSpec

moduleNameSpec :: Spec Unit
moduleNameSpec = do
  it "should encode module name" do
    modules <- getCSTModules <$> buildProject' (addModule "Foo.Bar" (pure unit))
    (getModuleName <$> modules) `shouldEqual` [ fooBarModuleName ]

  it "should reject invalid module name" do
    buildProjectErr (addModule "Foo!" (pure unit))
      `shouldContain` (InvalidModuleName "Foo!")

  it "should reject duplicate module names" do
    buildProjectErr (addModule "Foo" (pure unit) *> addModule "Foo" (pure unit))
      `shouldContain` (DuplicateModuleName "Foo")

getModuleName :: Module -> ModuleName
getModuleName (Module { moduleName }) = moduleName

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
