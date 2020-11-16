module CST.Simple.Internal.Binder
       ( Binder
       , runBinder
       , bndrWildcard
       , bndrVar
       , bndrNamed
       , bndrConstructor
       , bndrBoolean
       , bndrChar
       , bndrString
       , bndrNumber
       , bndrArray
       , bndrRecord
       , bndrTyped
       , bndrOp
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, mkName, mkQualName)
import CST.Simple.Internal.RecordLabeled (RecordLabeled, runRecordLabeled)
import CST.Simple.Internal.Type (Type, runType)
import CST.Simple.Names (TypedConstructorName(..))
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Language.PS.CST as CST

newtype Binder = Binder (ModuleBuilder CST.Binder)

runBinder :: Binder -> ModuleBuilder CST.Binder
runBinder (Binder mb) = mb

bndrWildcard :: Binder
bndrWildcard = Binder $ pure CST.BinderWildcard

bndrVar :: String -> Binder
bndrVar ident = Binder $ CST.BinderVar <$> mkName ident

bndrNamed :: String -> Binder -> Binder
bndrNamed ident' binder' = Binder ado
  ident <- mkName ident'
  binder <- runBinder binder'
  in CST.BinderNamed { ident, binder }

bndrConstructor :: String -> Array Binder -> Binder
bndrConstructor name' args' = Binder ado
  name <- map getNamePart <$> mkQualName name'
  args <- traverse runBinder args'
  in CST.BinderConstructor { name, args }

  where
    getNamePart (TypedConstructorName _ n') = n'

bndrBoolean :: Boolean -> Binder
bndrBoolean =
  Binder <<< pure <<< CST.BinderBoolean

bndrChar :: Char -> Binder
bndrChar =
  Binder <<< pure <<< CST.BinderChar

bndrString :: String -> Binder
bndrString = Binder <<< pure <<< CST.BinderString

bndrInt :: Int -> Binder
bndrInt =
  Binder <<< pure <<< CST.BinderNumber <<< Left

bndrNumber :: Number -> Binder
bndrNumber =
  Binder <<< pure <<< CST.BinderNumber <<< Right

bndrArray :: Array Binder -> Binder
bndrArray =
  Binder <<< map CST.BinderArray <<< traverse runBinder

bndrRecord :: Array (RecordLabeled Binder) -> Binder
bndrRecord bs =
  Binder $ CST.BinderRecord
  <$> traverse (traverse runBinder <=< runRecordLabeled) bs

bndrTyped :: Binder -> Type -> Binder
bndrTyped b t =
  Binder $ CST.BinderTyped <$> runBinder b <*> runType t

bndrOp :: Binder -> String -> Binder -> Binder
bndrOp b1 op b2 =
  Binder $ CST.BinderOp <$> runBinder b1 <*> mkQualName op <*> runBinder b2
