module CST.Simple.Internal.Kind
       ( Kind
       , runKind
       , knd
       , kndArrow
       , kndRow
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, mkQualName)
import Language.PS.CST as CST

newtype Kind = Kind (ModuleBuilder CST.Kind)

derive newtype instance eqKind :: Eq Kind
derive newtype instance ordKind :: Ord Kind

runKind :: Kind -> ModuleBuilder CST.Kind
runKind (Kind mb) = mb

knd :: String -> Kind
knd s = Kind $ CST.KindName <$> mkQualName s

kndArrow :: Kind -> Kind -> Kind
kndArrow k1 k2 = Kind $ CST.KindArr <$> runKind k1 <*> runKind k2

kndRow :: Kind -> Kind
kndRow k = Kind $ CST.KindRow <$> runKind k
