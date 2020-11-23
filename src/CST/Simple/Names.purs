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
       , nameP
       , mkNameError
       , readName
       , readName'
       , class UnwrapQualName
       , unwrapQualName
       , module E
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.Utils (exceptM)
import Control.Monad.Error.Class (class MonadThrow)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Char.Unicode as Char
import Data.Either (Either, hush, note)
import Data.Foldable (elem, foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as CodeUnits
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Language.PS.CST (Ident, Label(..), ModuleName, QualifiedName(..)) as E
import Language.PS.CST (Ident, ModuleName, QualifiedName)
import Language.PS.CST as CST
import Language.PS.CST.ReservedNames (isReservedName)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodeUnits (char, eof, regex, satisfy)
import Text.Parsing.StringParser.Combinators (many1)
import Type.Proxy (Proxy(..))

type TypeName = CST.ProperName CST.ProperNameType_TypeName
type ConstructorName = CST.ProperName CST.ProperNameType_ConstructorName
type ClassName = CST.ProperName CST.ProperNameType_ClassName
type KindName = CST.ProperName CST.ProperNameType_KindName
type Namespace = CST.ProperName CST.ProperNameType_Namespace

typeName' :: String -> Maybe TypeName
typeName' = runParser_ properNameP

typeNameP :: Parser TypeName
typeNameP = properNameP

constructorName' :: String -> Maybe ConstructorName
constructorName' = runParser_ constructorNameP

constructorNameP :: Parser ConstructorName
constructorNameP = properNameP

className' :: String -> Maybe ClassName
className' = runParser_ classNameP

classNameP :: Parser ClassName
classNameP = properNameP

kindName' :: String -> Maybe KindName
kindName' = runParser_ kindNameP

kindNameP :: Parser KindName
kindNameP = properNameP

-- properName' is used to create variants of ProperName
-- except for namespace which disallows `_` and `'`.
properNameP :: forall p. Parser (CST.ProperName p)
properNameP =
  CST.ProperName <$> regex "[A-Z][A-Za-z0-9_']*"

namespace' :: String -> Maybe Namespace
namespace' = runParser_ namespaceP

namespaceP :: Parser Namespace
namespaceP =
  CST.ProperName <$> regex "[A-Z][A-Za-z0-9]*"

-- TypedConstructorName

data TypedConstructorName = TypedConstructorName TypeName ConstructorName

instance typedConstructorNameShow :: Show TypedConstructorName where
  show (TypedConstructorName t c) = "(TypedConstructorName " <> show t <> " " <> show c <> ")"

derive instance typedConstructorNameEq :: Eq TypedConstructorName
derive instance typedConstructorNameOrd :: Ord TypedConstructorName

typedConstructorName' :: String -> Maybe TypedConstructorName
typedConstructorName' s = do
  runParser_ typedConstructorNameP s

typedConstructorNameP :: Parser TypedConstructorName
typedConstructorNameP =
  TypedConstructorName
  <$> typeNameP
  <*> (char '(' *> constructorNameP <* char ')')

-- ident

ident' :: String -> Maybe Ident
ident' =
  runParser_ identP

identP :: Parser CST.Ident
identP = do
  i <- regex "[a-z_][A-Za-z0-9_']*"
  when (isReservedName i) (Parser.fail "Ident was reserved name")
  pure $ CST.Ident i

-- opName

type TypeOpName = CST.OpName CST.OpNameType_TypeOpName

typeOpName' :: String -> Maybe TypeOpName
typeOpName' = opName'

type ValueOpName = CST.OpName CST.OpNameType_ValueOpName

valueOpName' :: String -> Maybe ValueOpName
valueOpName' = opName'

opName' :: forall p. String -> Maybe (CST.OpName p)
opName' = runParser_ opNameP

opNameP :: forall p. Parser (CST.OpName p)
opNameP = do
  cs <- many1 (satisfy isSymbolChar)
  pure $ CST.OpName $ foldMap CodeUnits.singleton cs

isSymbolChar :: Char -> Boolean
isSymbolChar c =
  -- https://github.com/purescript/purescript/blob/48634d05da1021ac1bcb4907461ae9b2d3b0e699/lib/purescript-cst/src/Language/PureScript/CST/Lexer.hs#L659
  c `elem` asciiSymbolChars
  || (not (Char.isAscii c) && Char.isSymbol c)

asciiSymbolChars :: Array Char
asciiSymbolChars = toCharArray ":!#$%&*+./<=>?@\\^|-~"

-- moduleName

moduleName' :: String -> Maybe ModuleName
moduleName' = runParser_ moduleNameP

moduleNameP :: Parser ModuleName
moduleNameP = do
  a <- namespaceP
  as <- Array.many $ char '.' *> namespaceP
  pure $ CST.ModuleName $ NonEmptyArray.cons' a as

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

    readNameQ u n =
      lmap
        (InvalidQualifiedName s u <<< Just)
        (readName n)

    readQualName u = readNameQ u =<< unwrap u

-- ReadName

class ReadName a where
  nameP :: Parser a
  mkNameError :: Proxy a -> String -> CodegenError

instance readNameModuleName :: ReadName ModuleName where
  nameP = moduleNameP
  mkNameError _ = InvalidModuleName

instance readNameTypeName :: ReadName (CST.ProperName CST.ProperNameType_TypeName) where
  nameP = typeNameP
  mkNameError _ = InvalidTypeName

instance readNameConstructorName :: ReadName (CST.ProperName CST.ProperNameType_ConstructorName) where
  nameP = constructorNameP
  mkNameError _ = InvalidConstructorName

instance readNameClassName :: ReadName (CST.ProperName CST.ProperNameType_ClassName) where
  nameP = classNameP
  mkNameError _ = InvalidClassName

instance readNameKindName :: ReadName (CST.ProperName CST.ProperNameType_KindName) where
  nameP = kindNameP
  mkNameError _ = InvalidKindName

instance readTypedConstructorName :: ReadName TypedConstructorName where
  nameP = typedConstructorNameP
  mkNameError _ = InvalidConstructorName

instance readNameIdent :: ReadName Ident where
  nameP = identP
  mkNameError _ = InvalidIdent

instance readNameTypeOpName :: ReadName (CST.OpName CST.OpNameType_TypeOpName) where
  nameP = opNameP
  mkNameError _ = InvalidTypeOpName

instance readNameValueOpName :: ReadName (CST.OpName CST.OpNameType_ValueOpName) where
  nameP = opNameP
  mkNameError _ = InvalidValueOpName

readName :: forall a. ReadName a => String -> Either CodegenError a
readName s =
  note (mkNameError (Proxy :: _ a) s) (runParser_ nameP s)

readName' :: forall m a. MonadThrow CodegenError m => ReadName a => String -> m a
readName' = exceptM <<< readName

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

runParser_ :: forall a. Parser a -> String -> Maybe a
runParser_ p = hush <<< runParser (p <* eof)
