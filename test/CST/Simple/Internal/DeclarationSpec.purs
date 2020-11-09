module CST.Simple.Internal.DeclarationSpec
       ( declarationSpec
       ) where

import Prelude

import CST.Simple.Internal.CommonOp ((*->), (*::))
import CST.Simple.Internal.Declaration (Declaration, dataCtor, declClass, declData, declNewtype, declType, runDeclaration)
import CST.Simple.Internal.Kind (knd)
import CST.Simple.Internal.Type (cnst, typVar)
import CST.Simple.Internal.TypeVarBinding (tvb)
import CST.Simple.TestUtils (cstUnqualProperName, shouldMatchCST)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Language.PS.CST as CST
import Test.Spec (Spec, it)

declarationSpec :: Spec Unit
declarationSpec = do
  it "should create data decl" do
    declData "Foo" [ tvb "a", "b" *:: knd "Symbol" ]
      [ dataCtor "Bar" []
      ] `shouldMatchCSTDecl`
      CST.DeclData
      { comments: Nothing
      , head: CST.DataHead
        { dataHdName: CST.ProperName "Foo"
        , dataHdVars:
          [ CST.TypeVarName (CST.Ident "a")
          , CST.TypeVarKinded (CST.Ident "b") (CST.KindName $ cstUnqualProperName "Symbol")
          ]
        }
      , constructors:
        [ CST.DataCtor
          { dataCtorName: CST.ProperName "Bar"
          , dataCtorFields: []
          }
        ]
      }

  it "should create type decl" do
    declType "Foo" [ tvb "a" ] (typVar "a")
      `shouldMatchCSTDecl`
      CST.DeclType
      { comments: Nothing
      , head: CST.DataHead
        { dataHdName: CST.ProperName "Foo"
        , dataHdVars:
          [ CST.TypeVarName (CST.Ident "a")
          ]
        }
      , type_: CST.TypeVar (CST.Ident "a")
      }

  it "should create newtype decl" do
    declNewtype "Foo" [ tvb "a" ] "Bar" (typVar "a")
      `shouldMatchCSTDecl`
      CST.DeclNewtype
      { comments: Nothing
      , head: CST.DataHead
        { dataHdName: CST.ProperName "Foo"
        , dataHdVars:
          [ CST.TypeVarName (CST.Ident "a")
          ]
        }
      , name: CST.ProperName "Bar"
      , type_: CST.TypeVar (CST.Ident "a")
      }

  it "should create class decl" do
    declClass "Foo" [ tvb "a", tvb "b" ] [ cnst "Bar" [] ] [ ["a"] /\ ["b"] ] [ "foo" /\ typVar "a" *-> typVar "b" ]
      `shouldMatchCSTDecl`
      CST.DeclClass
      { comments: Nothing
      , head:
        { fundeps:
          [ CST.FundepDetermines
            (NonEmptyArray.singleton (CST.Ident "a"))
            (NonEmptyArray.singleton (CST.Ident "b"))
          ]
        , name: CST.ProperName "Foo"
        , super:
          [ CST.Constraint
            { args: []
            , className: cstUnqualProperName "Bar"
            }
          ]
        , vars:
          [ CST.TypeVarName (CST.Ident "a")
          , CST.TypeVarName (CST.Ident "b")
          ]
        }
      , methods:
        [ { ident: (CST.Ident "foo")
          , type_: CST.TypeArr (CST.TypeVar (CST.Ident "a")) (CST.TypeVar (CST.Ident "b"))
          }
        ]
      }


shouldMatchCSTDecl :: forall m. MonadThrow Error m => Declaration -> CST.Declaration -> m Unit
shouldMatchCSTDecl = shouldMatchCST runDeclaration