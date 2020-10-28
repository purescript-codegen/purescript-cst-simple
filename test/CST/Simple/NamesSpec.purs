module CST.Simple.NamesSpec
       ( namesSpec
       ) where

import Prelude

import CST.Simple.Names (QualifiedName(..), ident', moduleName', opName', properName', qualNameOp, qualNameProper)
import CST.Simple.TestUtils (fooBarModuleName)
import Data.Maybe (Maybe(..), isJust, isNothing)
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

opNameSpec :: Spec Unit
opNameSpec = describe "opName" do
  it "should accept operator name" do
    opName' "<>" `shouldSatisfy` isJust

  it "should reject alphanum name" do
    opName' "foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    opName' "" `shouldSatisfy` isNothing

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
