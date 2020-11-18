module CST.Simple.Internal.TypeVarBinding
       ( TypeVarBinding
       , runTypeVarBinding
       , tvb
       , tvbKinded
       ) where

import Prelude

import CST.Simple.Internal.Kind (Kind, runKind)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, mkName)
import Language.PS.CST as CST

newtype TypeVarBinding =
  TypeVarBinding (ModuleBuilder CST.TypeVarBinding)

derive newtype instance typeVarBindingEq :: Eq TypeVarBinding
derive newtype instance typeVarBindingOrd :: Ord TypeVarBinding

instance typeVarBindingShow :: Show TypeVarBinding where
  show (TypeVarBinding mb) =
    "(TypeVarBinding " <> show mb <> ")"

runTypeVarBinding :: TypeVarBinding -> ModuleBuilder CST.TypeVarBinding
runTypeVarBinding (TypeVarBinding mb) = mb

tvb :: String -> TypeVarBinding
tvb ident = TypeVarBinding $ CST.TypeVarName <$> mkName ident

tvbKinded :: String -> Kind -> TypeVarBinding
tvbKinded ident kind_ =
  TypeVarBinding $ CST.TypeVarKinded <$> mkName ident <*> runKind kind_
