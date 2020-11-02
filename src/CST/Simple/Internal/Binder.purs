module CST.Simple.Internal.Binder
       ( Binder
       , runBinder
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

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName)
import CST.Simple.Internal.RecordLabeled (RecordLabeled, runRecordLabeled)
import CST.Simple.Internal.Type (Type, runType)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Language.PS.CST as CST

newtype Binder = Binder (ModuleBuilder CST.Binder)

runBinder :: forall m. Monad m => Binder -> ModuleBuilderT m CST.Binder
runBinder (Binder mb) = liftModuleBuilder mb

bndrVar :: String -> Binder
bndrVar s = Binder $ CST.BinderVar <$> mkName s

bndrNamed :: String -> Binder -> Binder
bndrNamed i b = Binder ado
  ident <- mkName i
  binder <- runBinder b
  in CST.BinderNamed { ident, binder }

bndrConstructor :: String -> Array Binder -> Binder
bndrConstructor n as = Binder ado
  name <- mkName n
  args <- traverse runBinder as
  in CST.BinderConstructor { name, args }

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
  Binder $ CST.BinderOp <$> runBinder b1 <*> mkName op <*> runBinder b2
