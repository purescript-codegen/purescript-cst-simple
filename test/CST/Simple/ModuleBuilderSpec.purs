module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import Test.Spec (Spec, describe)

moduleBuilderSpec :: Spec Unit
moduleBuilderSpec = describe "ModuleBuilder" do
  moduleNameSpec

moduleNameSpec :: Spec Unit
moduleNameSpec = do
  pure unit
