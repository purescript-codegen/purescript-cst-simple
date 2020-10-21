module CST.Simple.Internal.SList
       ( kind SList
       , SListCons(..)
       , SListNil(..)
       , SListProxy(..)
       , class SListMapping
       , class SListMap
       , class SListReflect
       , reflectSList
       , class SListReflect1
       , reflectSList1
       ) where

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

-- SList

foreign import kind SList
foreign import data SListCons :: Symbol -> SList -> SList
foreign import data SListNil :: SList

data SListProxy (sl :: SList) = SListProxy

-- SList Map

class SListMapping (m :: Type) (s :: Symbol) (s' :: Symbol) | m s -> s'

class SListMap (m :: Type) (l :: SList) (l' :: SList) | m l -> l'

instance slistMapNil :: SListMap m SListNil SListNil
instance slistMapCons ::
  ( SListMapping m s s'
  , SListMap m t t'
  ) => SListMap m (SListCons s t) (SListCons s' t')

-- SListReflect

class SListReflect (l :: SList) where
  reflectSList :: SListProxy l -> Array String

instance reflectSListNil :: SListReflect SListNil where
  reflectSList _ = []

instance reflectSListCons ::
  ( SListReflect t
  , IsSymbol h
  ) => SListReflect (SListCons h t) where
  reflectSList _ =
    Array.cons
    (reflectSymbol (SProxy :: _ h))
    (reflectSList (SListProxy :: _ t))

class SListReflect1 (l :: SList) where
  reflectSList1 :: SListProxy l -> NonEmptyArray String

instance sListReflect1Cons ::
  ( SListReflect t
  , IsSymbol h
  ) => SListReflect1 (SListCons h t) where
  reflectSList1 _ =
    NonEmptyArray.cons'
    (reflectSymbol (SProxy :: _ h))
    (reflectSList (SListProxy :: _ t))
