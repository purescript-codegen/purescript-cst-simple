module CST.Simple.Internal.TypeVarBinding
       ( TypeVarBinding
       , runTypeVarBinding
       , tvb
       , tvbKinded
       ) where

import Prelude

import CST.Simple.Internal.Kind (Kind, runKind)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder)
import Language.PS.CST as CST

newtype TypeVarBinding =
  TypeVarBinding (ModuleBuilder CST.TypeVarBinding)

runTypeVarBinding :: forall m. Monad m => TypeVarBinding -> ModuleBuilderT m CST.TypeVarBinding
runTypeVarBinding (TypeVarBinding mb) = liftModuleBuilder mb

tvb :: String -> TypeVarBinding
tvb i = TypeVarBinding $ pure $ CST.TypeVarName (CST.Ident i)

tvbKinded :: String -> Kind -> TypeVarBinding
tvbKinded i k =
  TypeVarBinding $ CST.TypeVarKinded (CST.Ident i) <$> runKind k
