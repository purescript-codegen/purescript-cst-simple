module CST.Simple.Names
       ( TypeName
       , ConstructorName
       , ClassName
       , KindName
       , typeName'
       , constructorName'
       , className'
       , kindName'
       , TypedConstructorName(..)
       , typedConstructorName'
       , ident'
       , TypeOpName
       , typeOpName'
       , ValueOpName
       , valueOpName'
       , moduleName'
       , qualName
       , class ReadName
       , readName
       , class UnwrapQualName
       , unwrapQualName
       , module E
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import Control.MonadPlus (guard)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap, lmap)
import Data.Char.Unicode as Char
import Data.Either (Either, note)
import Data.Foldable (all, elem)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Language.PS.CST (Ident, Label(..), ModuleName, QualifiedName(..)) as E
import Language.PS.CST (Ident, ModuleName, QualifiedName)
import Language.PS.CST as CST
import Type.Proxy (Proxy(..))

type TypeName = CST.ProperName CST.ProperNameType_TypeName
type ConstructorName = CST.ProperName CST.ProperNameType_ConstructorName
type ClassName = CST.ProperName CST.ProperNameType_ClassName
type KindName = CST.ProperName CST.ProperNameType_KindName
type Namespace = CST.ProperName CST.ProperNameType_Namespace

typeName' :: String -> Maybe TypeName
typeName' = properName'

constructorName' :: String -> Maybe ConstructorName
constructorName' = properName'

className' :: String -> Maybe ClassName
className' = properName'

kindName' :: String -> Maybe KindName
kindName' = properName'

-- properName' is used to create variants of ProperName
-- except for namespace which disallows `_` and `'`.
properName' :: forall p. String -> Maybe (CST.ProperName p)
properName' s =
  CST.ProperName <$> filterRegex properNameRegex s

properNameRegex :: Regex
properNameRegex =
  unsafeRegex "^[A-Z][A-Za-z0-9_']*$" RegexFlags.noFlags

namespace' :: String -> Maybe Namespace
namespace' s =
  CST.ProperName <$> filterRegex namespaceRegex s

namespaceRegex :: Regex
namespaceRegex =
  unsafeRegex "^[A-Z][A-Za-z0-9]*$" RegexFlags.noFlags

-- TypedConstructorName

data TypedConstructorName = TypedConstructorName TypeName ConstructorName

instance typedConstructorNameShow :: Show TypedConstructorName where
  show (TypedConstructorName t c) = "(TypedConstructorName " <> show t <> " " <> show c <> ")"

derive instance typedConstructorNameEq :: Eq TypedConstructorName
derive instance typedConstructorNameOrd :: Ord TypedConstructorName

typedConstructorName' :: String -> Maybe TypedConstructorName
typedConstructorName' s = do
  openParenNdx <- String.indexOf (String.Pattern "(") s
  let split1 = String.splitAt openParenNdx s
  typeName <- typeName' split1.before
  consName <- constructorName' =<< unParen split1.after
  pure $ TypedConstructorName typeName consName

-- ident

ident' :: String -> Maybe Ident
ident' s =
  CST.Ident <$> filterRegex identRegex s

identRegex :: Regex
identRegex =
  unsafeRegex "^[a-z_][A-Za-z0-9_']*$" RegexFlags.noFlags

-- opName

type TypeOpName = CST.OpName CST.OpNameType_TypeOpName

typeOpName' :: String -> Maybe TypeOpName
typeOpName' = opName'

type ValueOpName = CST.OpName CST.OpNameType_ValueOpName

valueOpName' :: String -> Maybe ValueOpName
valueOpName' = opName'

opName' :: forall p. String -> Maybe (CST.OpName p)
opName' s =
  guard ( not String.null s
          && all isSymbolChar (toCharArray s)
        )
  $> CST.OpName s

isSymbolChar :: Char -> Boolean
isSymbolChar c =
  -- https://github.com/purescript/purescript/blob/48634d05da1021ac1bcb4907461ae9b2d3b0e699/lib/purescript-cst/src/Language/PureScript/CST/Lexer.hs#L659
  c `elem` asciiSymbolChars
  || (not (Char.isAscii c) && Char.isSymbol c)

asciiSymbolChars :: Array Char
asciiSymbolChars = toCharArray ":!#$%&*+./<=>?@\\^|-~"

-- moduleName

moduleName' :: String -> Maybe ModuleName
moduleName' s =
  CST.ModuleName
  <$> ( NonEmptyArray.fromArray
        =<< traverse (namespace') (String.split (String.Pattern ".") s)
      )

-- TODO support period in opName

qualName ::
  forall a.
  UnwrapQualName a =>
  ReadName a =>
  String ->
  Either CodegenError (QualifiedName a)
qualName s = case String.lastIndexOf (String.Pattern ".") s of
  Just ndx -> ado
    qualModule <- Just <$> readQualModule (String.take ndx s)
    qualName <- readQualName $ String.drop (ndx + 1) s
    in CST.QualifiedName { qualModule, qualName }
  Nothing ->
    readName s <#> \n ->
    CST.QualifiedName { qualModule: Nothing, qualName: n }

  where
    readQualModule n =
      note (InvalidQualifiedModule s n) $ moduleName' n

    unwrap n =
      note (InvalidQualifiedName s n Nothing) $ unwrapQualName (Proxy :: _ a) n

    readName' u n =
      lmap
        (InvalidQualifiedName s u <<< Just)
        (readName n)

    readQualName u = readName' u =<< unwrap u


-- ReadName

class ReadName a where
  readName :: String -> Either CodegenError a

instance readNameTypeName :: ReadName (CST.ProperName CST.ProperNameType_TypeName) where
  readName s = note (InvalidTypeName s) (typeName' s)

instance readNameConstructorName :: ReadName (CST.ProperName CST.ProperNameType_ConstructorName) where
  readName s = note (InvalidConstructorName s) (constructorName' s)

instance readNameClassName :: ReadName (CST.ProperName CST.ProperNameType_ClassName) where
  readName s = note (InvalidClassName s) (className' s)

instance readNameKindName :: ReadName (CST.ProperName CST.ProperNameType_KindName) where
  readName s = note (InvalidKindName s) (kindName' s)

instance readTypedConstructorName :: ReadName TypedConstructorName where
  readName s = note (InvalidConstructorName s) (typedConstructorName' s)

instance readNameIdent :: ReadName Ident where
  readName s = note (InvalidIdent s) (ident' s)

instance readNameTypeOpName :: ReadName (CST.OpName CST.OpNameType_TypeOpName) where
  readName s = note (InvalidTypeOpName s) (typeOpName' s)

instance readNameValueOpName :: ReadName (CST.OpName CST.OpNameType_ValueOpName) where
  readName s = note (InvalidValueOpName s) (valueOpName' s)

instance readNameQualName ::
  ( UnwrapQualName a
  , ReadName a
  ) => ReadName (QualifiedName a) where
  readName = qualName

-- todo support type vars. add test when doing so
instance readNameDataHead :: ReadName CST.DataHead where
  readName s = bimap (InvalidDataHeadName s) toDataHead (readName s)
    where
      toDataHead dataHdName =
        CST.DataHead
        { dataHdName
        , dataHdVars: []
        }

-- UnwrapQualName

class UnwrapQualName a where
  unwrapQualName :: Proxy a -> String -> Maybe String

instance unwrapQualNameTypeName :: UnwrapQualName (CST.ProperName CST.ProperNameType_TypeName) where
  unwrapQualName _ = Just

instance unwrapQualNameConstructorName :: UnwrapQualName (CST.ProperName CST.ProperNameType_ConstructorName) where
  unwrapQualName _ = Just

instance unwrapQualNameClassName :: UnwrapQualName (CST.ProperName CST.ProperNameType_ClassName) where
  unwrapQualName _ = Just

instance unwrapQualNameKindName :: UnwrapQualName (CST.ProperName CST.ProperNameType_KindName) where
  unwrapQualName _ = Just

instance unwrapQualNameIdent :: UnwrapQualName Ident where
  unwrapQualName _ = Just

instance unwrapOpTypeName :: UnwrapQualName (CST.OpName CST.OpNameType_TypeOpName) where
  unwrapQualName _ = unParen

instance unwrapOpValueName :: UnwrapQualName (CST.OpName CST.OpNameType_ValueOpName) where
  unwrapQualName _ = unParen

instance unwrapTypedConstructorName :: UnwrapQualName TypedConstructorName where
  unwrapQualName _ = Just

-- Utils

filterRegex :: Regex -> String -> Maybe String
filterRegex re s =
  guard (Regex.test re s) $> s

unParen :: String -> Maybe String
unParen s = do
  let p1 = String.splitAt 1 s
  guard (p1.before == "(")
  let p2 = String.splitAt (String.length s - 2) p1.after
  guard (p2.after == ")")
  pure p2.before
