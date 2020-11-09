module Test.Main where

import Prelude

import CST.Simple.Internal.DeclarationSpec (declarationSpec)
import CST.Simple.Internal.ExpressionSpec (expressionSpec)
import CST.Simple.Internal.TypeSpec (typeSpec)
import CST.Simple.ModuleBuilderSpec (moduleBuilderSpec)
import CST.Simple.NamesSpec (namesSpec)
import CST.Simple.ProjectBuilderSpec (projectBuilderSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  namesSpec
  typeSpec
  expressionSpec
  declarationSpec
  projectBuilderSpec
  moduleBuilderSpec
