module CST.Simple.Internal.DeclSpec
       ( declSpec
       ) where

import Prelude

import CST.Simple.Internal.Declaration (Declaration, dataCtor, declData, runDeclaration)
import CST.Simple.Internal.Kind (knd)
import CST.Simple.Internal.TypeVarBinding (tvb, tvbKinded)
import CST.Simple.TestUtils (cstUnqualProperName, shouldMatchCST)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Language.PS.CST as CST
import Test.Spec (Spec, it)

declSpec :: Spec Unit
declSpec = do
  it "should create data decl" do
    declData "Foo" [ tvb "a", tvbKinded "b" (knd "Symbol") ]
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

shouldMatchCSTDecl :: forall m. MonadThrow Error m => Declaration -> CST.Declaration -> m Unit
shouldMatchCSTDecl = shouldMatchCST runDeclaration
