module CST.Simple.Internal.DeclarationSpec
       ( declarationSpec
       ) where

import Prelude

import CST.Simple.Internal.Binder (bndrVar)
import CST.Simple.Internal.CommonOp ((*->), (*::))
import CST.Simple.Internal.Declaration (Declaration, Fixity(..), dataCtor, declClass, declData, declDerive, declDeriveNewtype, declForeignData, declForeignKind, declForeignValue, declInfix, declInstance, declInstanceChain, declNewtype, declSignature, declType, declValue, fixityOpType, fixityOpValue, instanceBName, instanceBSig, instance_, runDeclaration)
import CST.Simple.Internal.Expression (exprIdent, exprInt, grd_)
import CST.Simple.Internal.Kind (knd)
import CST.Simple.Internal.Type (cnst, typCons, typVar)
import CST.Simple.Internal.TypeVarBinding (tvb)
import CST.Simple.TestUtils (cstUnqualIdent, cstUnqualProperName, shouldMatchCST)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
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

  it "should create single instance" do
    declInstance "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ]
      [ instanceBSig "foo" (typVar "a")
      , instanceBName "foo" [ bndrVar "a" ] (grd_ $ exprIdent "a")
      ]
      `shouldMatchCSTDecl`
      CST.DeclInstanceChain
      { comments: Nothing
      , instances: NonEmptyArray.singleton
        { head:
          { instClass: cstUnqualProperName "Foo"
          , instConstraints:
            [ CST.Constraint { className: cstUnqualProperName "Bar", args: [] }
            ]
          , instName: CST.Ident "fooI"
          , instTypes: NonEmptyArray.singleton
            (CST.TypeVar (CST.Ident "a"))
          }
        , body:
          [ CST.InstanceBindingSignature
            { ident: CST.Ident "foo"
            , type_: CST.TypeVar (CST.Ident "a")
            }
          , CST.InstanceBindingName
            { name: CST.Ident "foo"
            , binders: [ CST.BinderVar (CST.Ident "a") ]
            , guarded: CST.Unconditional { whereBindings: [], expr: CST.ExprIdent (cstUnqualIdent "a")}
            }
          ]
        }
      }

  it "should create instance chain" do
    declInstanceChain
      (instance_ "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ]
        [ instanceBSig "foo" (typVar "a")
        , instanceBName "foo" [ bndrVar "a" ] (grd_ $ exprIdent "a")
        ]
      ) []
      `shouldMatchCSTDecl`
      CST.DeclInstanceChain
      { comments: Nothing
      , instances: NonEmptyArray.singleton
        { head:
          { instClass: cstUnqualProperName "Foo"
          , instConstraints:
            [ CST.Constraint { className: cstUnqualProperName "Bar", args: [] }
            ]
          , instName: CST.Ident "fooI"
          , instTypes: NonEmptyArray.singleton
            (CST.TypeVar (CST.Ident "a"))
          }
        , body:
          [ CST.InstanceBindingSignature
            { ident: CST.Ident "foo"
            , type_: CST.TypeVar (CST.Ident "a")
            }
          , CST.InstanceBindingName
            { name: CST.Ident "foo"
            , binders: [ CST.BinderVar (CST.Ident "a") ]
            , guarded: CST.Unconditional { whereBindings: [], expr: CST.ExprIdent (cstUnqualIdent "a")}
            }
          ]
        }
      }

  it "should create derive decls" do
    declDerive "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ]
      `shouldMatchCSTDecl`
      CST.DeclDerive
      { comments: Nothing
      , deriveType: CST.DeclDeriveType_Odrinary
      , head:
        { instClass: cstUnqualProperName "Foo"
          , instConstraints:
            [ CST.Constraint { className: cstUnqualProperName "Bar", args: [] }
            ]
          , instName: CST.Ident "fooI"
          , instTypes: NonEmptyArray.singleton
            (CST.TypeVar (CST.Ident "a"))
          }
      }

  it "should create derive newtype decls" do
    declDeriveNewtype "fooI" [ cnst "Bar" [] ] "Foo" [ typVar "a" ]
      `shouldMatchCSTDecl`
      CST.DeclDerive
      { comments: Nothing
      , deriveType: CST.DeclDeriveType_Newtype
      , head:
        { instClass: cstUnqualProperName "Foo"
          , instConstraints:
            [ CST.Constraint { className: cstUnqualProperName "Bar", args: [] }
            ]
          , instName: CST.Ident "fooI"
          , instTypes: NonEmptyArray.singleton
            (CST.TypeVar (CST.Ident "a"))
          }
      }

  it "should create signature" do
    declSignature "foo" (typCons "Int")
      `shouldMatchCSTDecl`
      CST.DeclSignature
      { comments: Nothing
      , ident: CST.Ident "foo"
      , type_: CST.TypeConstructor (cstUnqualProperName "Int")
      }

  it "should create value decl" do
    declValue "foo" [ bndrVar "a" ] (grd_ (exprInt 3))
      `shouldMatchCSTDecl`
      CST.DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: CST.Ident "foo"
        , binders: [ CST.BinderVar (CST.Ident "a") ]
        , guarded: CST.Unconditional { whereBindings: []
                                     , expr: CST.ExprNumber (Left 3)
                                     }
        }
      }

  it "should create infix value ident" do
    declInfix Infixl 5 (fixityOpValue "foo" "<+>")
     `shouldMatchCSTDecl`
      CST.DeclFixity
      { comments: Nothing
      , fixityFields:
        { keyword: CST.Infixl
        , precedence: 5
        , operator: CST.FixityValue (Left (cstUnqualIdent "foo")) (CST.OpName "<+>")
        }
      }

  it "should create infix value proper name" do
    declInfix Infix 5 (fixityOpValue "Foo.Bar.Baz(Baz)" "<+>")
     `shouldMatchCSTDecl`
      CST.DeclFixity
      { comments: Nothing
      , fixityFields:
        { keyword: CST.Infix
        , precedence: 5
        , operator: CST.FixityValue (Right (cstUnqualProperName "Baz")) (CST.OpName "<+>")
        }
      }

  it "should create infix value type" do
    declInfix Infix 5 (fixityOpType "Foo.Bar.Baz" "<+>")
     `shouldMatchCSTDecl`
      CST.DeclFixity
      { comments: Nothing
      , fixityFields:
        { keyword: CST.Infix
        , precedence: 5
        , operator: CST.FixityType (cstUnqualProperName "Baz") (CST.OpName "<+>")
        }
      }

  it "should create foreign value" do
    declForeignValue "foo" (typCons "Int")
      `shouldMatchCSTDecl`
      CST.DeclForeign
      { comments: Nothing
      , foreign_: CST.ForeignValue
        { ident: CST.Ident "foo"
        , type_: CST.TypeConstructor (cstUnqualProperName "Int")
        }
      }

  it "should create foreign data" do
    declForeignData "Foo" (knd "Bar")
      `shouldMatchCSTDecl`
      CST.DeclForeign
      { comments: Nothing
      , foreign_: CST.ForeignData
        { name: CST.ProperName "Foo"
        , kind_: CST.KindName (cstUnqualProperName "Bar")
        }
      }

  it "should create foreign kind" do
    declForeignKind "Foo"
      `shouldMatchCSTDecl`
      CST.DeclForeign
      { comments: Nothing
      , foreign_: CST.ForeignKind
        { name: CST.ProperName "Foo"
        }
      }

shouldMatchCSTDecl :: forall m. MonadThrow Error m => Declaration -> CST.Declaration -> m Unit
shouldMatchCSTDecl = shouldMatchCST runDeclaration
