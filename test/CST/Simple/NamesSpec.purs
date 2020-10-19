module CST.Simple.NamesSpec
       ( namesSpec
       ) where

import Prelude

import CST.Simple.Names (iname', inameP, opName', opNameP, pname', pnameP)
import Data.Maybe (isJust, isNothing)
import Data.Symbol (SProxy(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

namesSpec :: Spec Unit
namesSpec = describe "names" do
  pnameSpec
  inameSpec
  opNameSpec

pnameSpec :: Spec Unit
pnameSpec = describe "pname" do
  it "should accept capitalized name" do
    pname' "Foo" `shouldSatisfy` isJust

  it "should reject noncapitalized name" do
    pname' "foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    pname' "" `shouldSatisfy` isNothing

  it "should accept typelevel proper names" do
    let _ = pnameP (SProxy :: _ "Foo")
        _ = pnameP (SProxy :: _ "F")
        -- should not compile:
        -- _ = pnameP (SProxy :: _ "foo")
        -- _ = pnameP (SProxy :: _ "F!")
    pure unit

inameSpec :: Spec Unit
inameSpec = describe "iname" do
  it "should accept noncapitalized name" do
    iname' "foo" `shouldSatisfy` isJust

  it "should reject capitalized name" do
    iname' "Foo" `shouldSatisfy` isNothing

  it "should reject empty name" do
    iname' "" `shouldSatisfy` isNothing

  it "should accept typelevel ident names" do
    let _ = inameP (SProxy :: _ "foo")
        _ = inameP (SProxy :: _ "f")
        -- should not compile:
        -- _ = inameP (SProxy :: _ "Foo")
        -- _ = inameP (SProxy :: _ "f!")
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
