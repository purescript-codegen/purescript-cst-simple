module CST.Simple.Names
       ( TypeName
       , ConstructorName
       , ClassName
       , KindName
       , Namespace
       , properName'
       , properNameP
       , ident'
       , identP
       , opName'
       , opNameP
       , moduleName'
       , moduleNameP
       , ModuleNameMapping
       , qualNameProper
       , qualNameIdent
       , qualNameOp
       , module E
       ) where

import Prelude

import CST.Simple.Internal.SList (class SListMap, class SListMapping, class SListReflect1, SListProxy(..), reflectSList1)
import CST.Simple.Internal.SymbolUtils (class NameFormat, class SplitWithChar, type (:/), CCLNil, DigitChar, LowercaseChar, OperatorChar, QuoteChar, UnderscoreChar, UppercaseChar)
import Control.MonadPlus (guard)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char.Unicode as Char
import Data.Foldable (all, elem)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Traversable (traverse)
import Language.PS.CST (Ident, ModuleName, OpName, ProperName, QualifiedName(..)) as E
import Language.PS.CST (Ident, OpName(..), ProperName, QualifiedName)
import Language.PS.CST as CST

type TypeName = CST.ProperName CST.ProperNameType_TypeName
type ConstructorName = CST.ProperName CST.ProperNameType_ConstructorName
type ClassName = CST.ProperName CST.ProperNameType_ClassName
type KindName = CST.ProperName CST.ProperNameType_KindName
type Namespace = CST.ProperName CST.ProperNameType_Namespace

properName' :: forall p. String -> Maybe (CST.ProperName p)
properName' s =
  CST.ProperName <$> filterRegex properNameRegex s

properNameP ::
  forall s p.
  NameFormat
  (UppercaseChar :/ CCLNil)
  (UppercaseChar :/ LowercaseChar :/ DigitChar :/ UnderscoreChar :/ QuoteChar :/ CCLNil)
  s =>
  IsSymbol s =>
  SProxy s ->
  ProperName p
properNameP = CST.ProperName <<< reflectSymbol

properNameRegex :: Regex
properNameRegex =
  unsafeRegex "^[A-Z][A-Za-z0-9_']*$" RegexFlags.noFlags

-- ident

ident' :: String -> Maybe Ident
ident' s =
  CST.Ident <$> filterRegex identRegex s

identP ::
  forall s.
  NameFormat
  (LowercaseChar :/ UnderscoreChar :/ CCLNil)
  (UppercaseChar :/ LowercaseChar :/ DigitChar :/ UnderscoreChar :/ QuoteChar :/ CCLNil)
  s =>
  IsSymbol s =>
  SProxy s ->
  Ident
identP = CST.Ident <<< reflectSymbol

identRegex :: Regex
identRegex =
  unsafeRegex "^[a-z_][A-Za-z0-9_]*$" RegexFlags.noFlags

-- opName

unsafeOpName :: forall p. String -> OpName p
unsafeOpName = OpName

opName' :: forall p. String -> Maybe (OpName p)
opName' s =
  guard ( not String.null s
          && all isSymbolChar (toCharArray s)
        )
  $> OpName s

-- currently does not support non ascii symbols
opNameP ::
  forall s p.
  NameFormat
  (OperatorChar :/ CCLNil)
  (OperatorChar :/ CCLNil)
  s =>
  IsSymbol s =>
  SProxy s ->
  OpName p
opNameP = OpName <<< reflectSymbol

isSymbolChar :: Char -> Boolean
isSymbolChar c =
  -- https://github.com/purescript/purescript/blob/48634d05da1021ac1bcb4907461ae9b2d3b0e699/lib/purescript-cst/src/Language/PureScript/CST/Lexer.hs#L659
  c `elem` asciiSymbolChars
  || (not (Char.isAscii c) && Char.isSymbol c)

asciiSymbolChars :: Array Char
asciiSymbolChars = toCharArray ":!#$%&*+./<=>?@\\^|-~"

-- moduleName

type ModuleName = CST.ModuleName

moduleName' :: String -> Maybe ModuleName
moduleName' s =
  CST.ModuleName
  <$> ( NonEmptyArray.fromArray
        -- TODO not all proper name chars allowed
        -- in particular, no single quoute
        =<< traverse (properName') (String.split (String.Pattern ".") s)
      )

data ModuleNameMapping

instance moduleNameMapping ::
  ( NameFormat
    (UppercaseChar :/ CCLNil)
    (UppercaseChar :/ LowercaseChar :/ DigitChar :/ CCLNil)
    s
  ) => SListMapping ModuleNameMapping s s

moduleNameP ::
  forall s l.
  SplitWithChar "." s l =>
  SListMap ModuleNameMapping l l =>
  SListReflect1 l =>
  SProxy s ->
  ModuleName
moduleNameP _ =
  CST.ModuleName $ CST.ProperName <$> reflectSList1 (SListProxy :: _ l)

-- qualName

qualNameProper :: forall p. String -> Maybe (QualifiedName (ProperName p))
qualNameProper = qualName' properName'

qualNameIdent :: String -> Maybe (CST.QualifiedName Ident)
qualNameIdent = qualName' ident'

-- TODO support period in opName
qualNameOp :: forall p. String -> Maybe (CST.QualifiedName (OpName p))
qualNameOp =  qualName' (opName' <=< unParen)
  where
    unParen s = do
      let p1 = String.splitAt 1 s
      guard (p1.before == "(")
      let p2 = String.splitAt (String.length s - 2) p1.after
      guard (p2.after == ")")
      pure p2.before

qualName' :: forall a. (String -> Maybe a) -> String -> Maybe (CST.QualifiedName a)
qualName' f s = do
  CST.QualifiedName q <- qualName s
  s' <- f q.qualName
  pure $ CST.QualifiedName { qualModule: q.qualModule
                           , qualName: s'
                           }

qualName :: String -> Maybe (CST.QualifiedName String)
qualName s = case String.lastIndexOf (String.Pattern ".") s of
  Just ndx -> ado
    qualModule <- Just <$> moduleName' (String.take ndx s)
    in CST.QualifiedName { qualModule, qualName: String.drop (ndx + 1) s }
  Nothing ->
    Just $ CST.QualifiedName { qualModule: Nothing, qualName: s }


-- Utils

filterRegex :: Regex -> String -> Maybe String
filterRegex re s =
  guard (Regex.test re s) $> s
