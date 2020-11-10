module CST.Simple.ModuleBuilderSpec
       ( moduleBuilderSpec
       ) where

import Prelude

import CST.Simple.Internal.Type (typ)
import CST.Simple.ModuleBuilder (addTypeDecl)
import CST.Simple.TestUtils (build, intCSTType, requireOne)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Language.PS.CST as CST
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)

moduleBuilderSpec :: Spec Unit
moduleBuilderSpec = describe "ModuleBuilder" do
  typeDeclarationSpec

typeDeclarationSpec :: Spec Unit
typeDeclarationSpec = do
  it "should accept type declarations" do
    mod <- build (addTypeDecl "X" [] (typ "Int"))
    mod.declarations `shouldContain`
      CST.DeclType
        { comments: Nothing
        , head: CST.DataHead
          { dataHdName: CST.ProperName "X"
          , dataHdVars: []
          }
        , type_: intCSTType
        }

  it "should not duplicate imports" do
    mod <- build (addTypeDecl "X" [] (typ "Foo.Bar.Baz") *> addTypeDecl "Y" [] (typ "Foo.Bar.Baz"))
    CST.ImportDecl { names } <- requireOne mod.imports
    Array.length names `shouldEqual` 1
