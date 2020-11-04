module CST.Simple.Internal.NamedBinders
       ( NamedBinders(..)
       , namedBinders1
       , nbAddBinder
       ) where

import CST.Simple.Internal.Binder (Binder)
import Data.Array as Array

data NamedBinders = NamedBinders String (Array Binder)

namedBinders1 :: String -> Binder -> NamedBinders
namedBinders1 n b = NamedBinders n [b]

nbAddBinder :: NamedBinders -> Binder -> NamedBinders
nbAddBinder (NamedBinders n bs) b =
  NamedBinders n (Array.cons b bs)
