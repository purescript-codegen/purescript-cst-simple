module CST.Simple.TestUtils
       ( fooBarModuleName
       ) where

import Prelude

import CST.Simple.Names (ModuleName(..))
import Data.Array.NonEmpty as NonEmptyArray
import Language.PS.CST as CST

fooBarModuleName :: ModuleName
fooBarModuleName =
  ModuleName $ NonEmptyArray.cons' (CST.ProperName "Foo") [ CST.ProperName "Bar" ]
