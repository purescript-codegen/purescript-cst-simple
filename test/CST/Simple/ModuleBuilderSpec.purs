module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import CST.Simple.ModuleBuilder (buildModule)
import CST.Simple.Types (CodegenError(..), getModuleName)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Language.PS.CST (ModuleName(..), ProperName(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

moduleBuilderSpec :: Spec Unit
moduleBuilderSpec = describe "ModuleBuilder" do
  moduleNameSpec

moduleNameSpec :: Spec Unit
moduleNameSpec = do
  it "should encode module name" do
    (getModuleName <$> buildModule "Foo.Bar" (pure unit))
      `shouldEqual` Right fooBarModuleName

  it "should reject invalid module name" do
    buildModule "Foo!" (pure unit)
      `shouldEqual` Left (InvalidModuleName "Foo!")

fooBarModuleName :: ModuleName
fooBarModuleName =
  ModuleName $ NonEmptyArray.cons' (ProperName "Foo") [ ProperName "Bar" ]
