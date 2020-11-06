module CST.Simple.Internal.Kind
       ( Kind
       , runKind
       , knd
       , kndArrow
       , kndRow
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkQualName)
import Language.PS.CST as CST

newtype Kind = Kind (ModuleBuilder CST.Kind)

runKind :: forall m. Monad m => Kind -> ModuleBuilderT m CST.Kind
runKind (Kind mb) = liftModuleBuilder mb

knd :: String -> Kind
knd s = Kind $ CST.KindName <$> mkQualName s

kndArrow :: Kind -> Kind -> Kind
kndArrow k1 k2 = Kind $ CST.KindArr <$> runKind k1 <*> runKind k2

kndRow :: Kind -> Kind
kndRow k = Kind $ CST.KindRow <$> runKind k
