module CST.Simple.NameFormat
       ( NameFormat(..)
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data NameFormat
  = NFModuleName
  | NFTypeName
  | NFTypedConstructorName
  | NFClassName
  | NFKindName
  | NFIdent
  | NFTypeOpName
  | NFValueOpName

derive instance nameFormatEq :: Eq NameFormat
derive instance nameFormatOrd :: Ord NameFormat
derive instance nameFormatGeneric :: Generic NameFormat _
instance nameFormatShow :: Show NameFormat where
  show = genericShow
