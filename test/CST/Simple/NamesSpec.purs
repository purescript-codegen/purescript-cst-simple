module CST.Simple.NamesSpec
       ( namesSpec
       ) where

import Prelude

import CST.Simple.Names (QualifiedName(..), ident', identP, moduleName', moduleNameP, opName', opNameP, properName', properNameP, qualNameOp, qualNameProper)
import CST.Simple.TestUtils (fooBarModuleName)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Symbol (SProxy(..))
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

namesSpec :: Spec Unit
namesSpec = describe "names" do
  pnameSpec
  inameSpec
  opNameSpec
  moduleNameSpec
  qualNameSpec

pnameSpec :: Spec Unit
pnameSpec = describe "pname" do
  it "should accept capitalized name" do
    properName' "Foo" `shouldSatisfy` isJust
    properName' "Foo'" `shouldSatisfy` isJust
    properName' "Foo_" `shouldSatisfy` isJust

  it "should reject noncapitalized name" do
    properName' "foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    properName' "" `shouldSatisfy` isNothing

  it "should accept typelevel proper names" do
    let _ = properNameP (SProxy :: _ "Foo")
        _ = properNameP (SProxy :: _ "Foo'")
        _ = properNameP (SProxy :: _ "Foo_")
        -- should not compile:
        -- _ = properNameP (SProxy :: _ "foo")
        -- _ = properNameP (SProxy :: _ "F!")
    pure unit

inameSpec :: Spec Unit
inameSpec = describe "iname" do
  it "should accept noncapitalized name" do
    ident' "foo" `shouldSatisfy` isJust

  it "should accept preceeding underscore" do
    ident' "_foo" `shouldSatisfy` isJust

  it "should reject capitalized name" do
    ident' "Foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    ident' "" `shouldSatisfy` isNothing

  it "should accept typelevel ident names" do
    let _ = identP (SProxy :: _ "foo")
        _ = identP (SProxy :: _ "_foo")
        -- should not compile:
        -- _ = identP (SProxy :: _ "Foo")
        -- _ = identP (SProxy :: _ "f!")
    pure unit

opNameSpec :: Spec Unit
opNameSpec = describe "opName" do
  it "should accept operator name" do
    opName' "<>" `shouldSatisfy` isJust

  it "should reject alphanum name" do
    opName' "foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    opName' "" `shouldSatisfy` isNothing

  it "should accept typelevel operator names" do
    let _ = opNameP (SProxy :: _ "<>")
        _ = opNameP (SProxy :: _ "<")
        -- should not compile:
        -- _ = opNameP (SProxy :: _ "Foo")
        -- _ = opNameP (SProxy :: _ "f!")
    pure unit

moduleNameSpec :: Spec Unit
moduleNameSpec = describe "moduleName" do
  it "should accept single proper name" do
    moduleName' "Foo" `shouldSatisfy` isJust

  it "should accept multiple proper name" do
    moduleName' "Foo.Bar" `shouldSatisfy` isJust

  it "should reject non proper name segments" do
    moduleName' "Foo.bar" `shouldSatisfy` isNothing
    moduleName' "Foo..Bar" `shouldSatisfy` isNothing
    moduleName' "Foo." `shouldSatisfy` isNothing

  it "should reject empty name" do
    moduleName' "" `shouldSatisfy` isNothing

  it "should accept typelevel module names" do
    Just (moduleNameP (SProxy :: _ "Foo")) `shouldEqual` moduleName' "Foo"
    Just (moduleNameP (SProxy :: _ "Foo.Bar")) `shouldEqual` moduleName' "Foo.Bar"

    -- should not compile:
    -- let
      -- _ = moduleNameP (SProxy :: _ "")
      -- _ = moduleNameP (SProxy :: _ "Foo.bar")
      -- _ = moduleNameP (SProxy :: _ "Foo..Bar")
    pure unit

qualNameSpec :: Spec Unit
qualNameSpec = describe "qualName" do
  it "should reject empty" do
    (qualNameProper "") `shouldSatisfy` isNothing
    (qualNameProper ".") `shouldSatisfy` isNothing

  it "should accept unqualified properName" do
    (qualNameProper "Foo") `shouldEqual`
      Just (QualifiedName { qualModule: Nothing, qualName: CST.ProperName "Foo" })

  it "should read qualified properName" do
    (qualNameProper "Foo.Bar.Baz") `shouldEqual`
      Just (QualifiedName { qualModule: Just fooBarModuleName, qualName: CST.ProperName "Baz" })

  it "should accept unqualified oproperName" do
    (qualNameOp "(<>)") `shouldEqual`
      Just (QualifiedName { qualModule: Nothing, qualName: CST.OpName "<>" })

  it "should read qualified oproperName" do
    (qualNameOp "Foo.Bar.(<>)") `shouldEqual`
      Just (QualifiedName { qualModule: Just fooBarModuleName, qualName: CST.OpName "<>" })
