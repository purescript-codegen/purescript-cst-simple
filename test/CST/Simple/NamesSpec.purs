module CST.Simple.NamesSpec
       ( namesSpec
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Names (QualifiedName(..), TypeName, TypeOpName, TypedConstructorName(..), className', constructorName', ident', kindName', moduleName', qualName, typeName', typeOpName', typedConstructorName')
import CST.Simple.TestUtils (fooBarModuleName)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

namesSpec :: Spec Unit
namesSpec = describe "names" do
  properNameSpec "typeName" typeName'
  properNameSpec "constructorName" constructorName'
  properNameSpec "className" className'
  properNameSpec "kindName" kindName'
  typedConstructorNameSpec
  identSpec
  typeOpNameSpec
  moduleNameSpec
  qualNameSpec

properNameSpec ::
  forall p.
  Show (CST.ProperName p) =>
  String ->
  (String -> Maybe (CST.ProperName p)) ->
  Spec Unit
properNameSpec label properName' = describe label do
  it "should accept capitalized name" do
    properName' "Foo" `shouldSatisfy` isJust
    properName' "Foo'" `shouldSatisfy` isJust
    properName' "Foo_" `shouldSatisfy` isJust

  it "should reject noncapitalized name" do
    properName' "foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    properName' "" `shouldSatisfy` isNothing

typedConstructorNameSpec :: Spec Unit
typedConstructorNameSpec = describe "typed constructor name" do
  it "should accept typed constructor name" do
    typedConstructorName' "Foo(Bar)" `shouldEqual` Just (TypedConstructorName (CST.ProperName "Foo") (CST.ProperName "Bar"))

  it "should reject empty" do
    typedConstructorName' "" `shouldSatisfy` isNothing

  it "should reject missing closing paren" do
    typedConstructorName' "Foo(Bar" `shouldSatisfy` isNothing

  it "should reject missing wrong type name" do
    typedConstructorName' "foo(Bar)" `shouldSatisfy` isNothing

  it "should reject missing wrong constructor name" do
    typedConstructorName' "Foo(bar)" `shouldSatisfy` isNothing

  it "should reject missing empty type name" do
    typedConstructorName' "(bar)" `shouldSatisfy` isNothing

  it "should reject missing empty constructor name" do
    typedConstructorName' "Foo()" `shouldSatisfy` isNothing

identSpec :: Spec Unit
identSpec = describe "ident" do
  it "should accept noncapitalized name" do
    ident' "foo" `shouldSatisfy` isJust

  it "should accept preceeding underscore" do
    ident' "_foo" `shouldSatisfy` isJust

  it "should reject capitalized name" do
    ident' "Foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    ident' "" `shouldSatisfy` isNothing

typeOpNameSpec :: Spec Unit
typeOpNameSpec = describe "opName" do
  it "should accept operator name" do
    typeOpName' "<>" `shouldSatisfy` isJust

  it "should reject alphanum name" do
    typeOpName' "foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    typeOpName' "" `shouldSatisfy` isNothing

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

  it "should reject single quotes" do
    moduleName' "Foo'" `shouldSatisfy` isNothing

  it "should reject single underscore" do
    moduleName' "Foo_" `shouldSatisfy` isNothing

qualNameSpec :: Spec Unit
qualNameSpec = describe "qualName" do
  it "should reject empty" do
    (qualNameType "") `shouldEqual` Left (InvalidTypeName "")
    (qualNameType ".") `shouldEqual` Left (InvalidQualifiedModule "." "")

  it "should reject invalid module prefix" do
    (qualNameType "Foo'.Baz") `shouldEqual` Left (InvalidQualifiedModule "Foo'.Baz" "Foo'")

  it "should reject invalid qualified name wrapper" do
    -- should have had parenthesis
    (qualNameOp "Foo.+") `shouldEqual` Left (InvalidQualifiedName "Foo.+" "+" Nothing)

  it "should reject invalid unqualified name wrapper" do
    (qualNameOp "Foo") `shouldEqual` Left (InvalidTypeOpName "Foo")

  it "should accept unqualified properName" do
    (qualNameType "Foo") `shouldEqual`
      Right (QualifiedName { qualModule: Nothing, qualName: CST.ProperName "Foo" })

  it "should read qualified properName" do
    (qualNameType "Foo.Bar.Baz") `shouldEqual`
      Right (QualifiedName { qualModule: Just fooBarModuleName, qualName: CST.ProperName "Baz" })

  -- todo differentiate between type op and op
  it "should accept unqualified opName" do
    (qualNameOp "<>") `shouldEqual`
      Right (QualifiedName { qualModule: Nothing, qualName: CST.OpName "<>" })

  it "should read qualified opName" do
    (qualNameOp "Foo.Bar.(<>)") `shouldEqual`
      Right (QualifiedName { qualModule: Just fooBarModuleName, qualName: CST.OpName "<>" })

-- todo opName with period

qualNameType :: String -> Either CodegenError (QualifiedName TypeName)
qualNameType = qualName

qualNameOp :: String -> Either CodegenError (QualifiedName TypeOpName)
qualNameOp = qualName
