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
       , class ReadImportName
       , importNameP
       , class AsNameFormat
       , asNameFormat
       , readName
       , readName'
       , module E
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.Utils (exceptM)
import CST.Simple.NameFormat (NameFormat(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Char.Unicode as Char
import Data.Either (Either, hush)
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
import Text.Parsing.StringParser (ParseError(..), Parser, runParser, try, unParser)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodeUnits (char, eof, regex, satisfy, skipSpaces, string)
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
  ReadName a =>
  ReadImportName a =>
  AsNameFormat a =>
  String ->
  Either CodegenError (QualifiedName a)
qualName =
  runParser' true (asNameFormat (Proxy :: _ a)) qualNameP

qualNameP ::
  forall a.
  ReadName a =>
  ReadImportName a =>
  Parser (QualifiedName a)
qualNameP = try qualifiedP <|> unqualifiedP
  where
    unqualifiedP = nameP <#> \name ->
      CST.QualifiedName { qualModule: Nothing, qualName: name }

    qualifiedP = ado
      qualModule <- Just <$> moduleNameP
      qualName <- char '(' *> importNameP <* char ')'
      in CST.QualifiedName { qualModule, qualName }

-- ReadName

class ReadName a where
  nameP :: Parser a

instance readNameModuleName :: ReadName ModuleName where
  nameP = moduleNameP

instance readNameTypeName :: ReadName (CST.ProperName CST.ProperNameType_TypeName) where
  nameP = typeNameP

instance readNameConstructorName :: ReadName (CST.ProperName CST.ProperNameType_ConstructorName) where
  nameP = constructorNameP

instance readNameClassName :: ReadName (CST.ProperName CST.ProperNameType_ClassName) where
  nameP = classNameP

instance readNameKindName :: ReadName (CST.ProperName CST.ProperNameType_KindName) where
  nameP = kindNameP

instance readNameTypedConstructorName :: ReadName TypedConstructorName where
  nameP = typedConstructorNameP

instance readNameIdent :: ReadName Ident where
  nameP = identP

instance readNameTypeOpName :: ReadName (CST.OpName CST.OpNameType_TypeOpName) where
  nameP = opNameP

instance readNameValueOpName :: ReadName (CST.OpName CST.OpNameType_ValueOpName) where
  nameP = opNameP

--

class ReadImportName a where
  importNameP :: Parser a

instance readImportNameTypeName :: ReadImportName (CST.ProperName CST.ProperNameType_TypeName) where
  importNameP = nameP

instance readImportNameConstructorName :: ReadImportName (CST.ProperName CST.ProperNameType_ConstructorName) where
  importNameP = nameP

instance readImportNameClassName :: ReadImportName (CST.ProperName CST.ProperNameType_ClassName) where
  importNameP = string "class" *> skipSpaces *> nameP

instance readImportNameKindName :: ReadImportName (CST.ProperName CST.ProperNameType_KindName) where
  importNameP = string "kind" *> skipSpaces *> nameP

instance readImportNameTypedConstructorName :: ReadImportName TypedConstructorName where
  importNameP = nameP

instance readImportNameIdent :: ReadImportName Ident where
  importNameP = nameP

instance readImportNameTypeOpName :: ReadImportName (CST.OpName CST.OpNameType_TypeOpName) where
  importNameP = string "type" *> skipSpaces *> (char '(' *> nameP <* char ')')

instance readImportNameValueOpName :: ReadImportName (CST.OpName CST.OpNameType_ValueOpName) where
  importNameP = (char '(' *> nameP <* char ')')

--

class AsNameFormat a where
  asNameFormat :: Proxy a -> NameFormat

instance asNameFormatModuleName :: AsNameFormat ModuleName where
  asNameFormat _ = NFModuleName

instance asNameFormatTypeName :: AsNameFormat (CST.ProperName CST.ProperNameType_TypeName) where
  asNameFormat _ = NFTypeName

instance asNameFormatConstructorName :: AsNameFormat (CST.ProperName CST.ProperNameType_ConstructorName) where
  asNameFormat _ = NFTypedConstructorName

instance asNameFormatClassName :: AsNameFormat (CST.ProperName CST.ProperNameType_ClassName) where
  asNameFormat _ = NFClassName

instance asNameFormatKindName :: AsNameFormat (CST.ProperName CST.ProperNameType_KindName) where
  asNameFormat _ = NFKindName

instance asNameFormatdConstructorName :: AsNameFormat TypedConstructorName where
  asNameFormat _ = NFTypedConstructorName

instance asNameFormatIdent :: AsNameFormat Ident where
  asNameFormat _ = NFIdent

instance asNameFormatTypeOpName :: AsNameFormat (CST.OpName CST.OpNameType_TypeOpName) where
  asNameFormat _ = NFTypeOpName

instance asNameFormatValueOpName :: AsNameFormat (CST.OpName CST.OpNameType_ValueOpName) where
  asNameFormat _ = NFValueOpName

readName ::
  forall a.
  ReadName a =>
  AsNameFormat a =>
  String ->
  Either CodegenError a
readName s =
  runParser' false (asNameFormat (Proxy :: _ a)) nameP s

readName' ::
  forall m a.
  MonadThrow CodegenError m =>
  ReadName a =>
  AsNameFormat a =>
  String ->
  m a
readName' = exceptM <<< readName

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

runParser' ::
  forall a.
  Boolean ->
  NameFormat ->
  Parser a ->
  String ->
  Either CodegenError a
runParser' allowQualified nameFormat p str = bimap
  (\{ error: ParseError msg, pos } ->
    InvalidName { given: str, msg, pos, allowQualified, nameFormat })
  _.result
  $ unParser (p <* eof) { pos: 0, str }
