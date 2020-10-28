module CST.Simple.Names
       ( PName
       , unsafePName
       , pname'
       , pnameP
       , pnameToProperName
       , pnameToString
       , IName
       , iname'
       , inameP
       , inameToIdent
       , inameToString
       , OpName
       , unsafeOpName
       , opName'
       , opNameP
       , opNameToOpName
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
import Language.PS.CST (ModuleName(..), QualifiedName(..)) as E
import Language.PS.CST (ModuleName(..), ProperName(..))
import Language.PS.CST as CST

-- pname

newtype PName = PName String

derive newtype instance pnameEq :: Eq PName
derive newtype instance pnameOrd :: Ord PName

instance pnameShow :: Show PName where
  show (PName s) = "(PName " <> show s <> ")"

unsafePName :: String -> PName
unsafePName = PName

pname' :: String -> Maybe PName
pname' s =
  PName <$> filterRegex pnameRegex s

pnameP ::
  forall s.
  NameFormat
  (UppercaseChar :/ CCLNil)
  (UppercaseChar :/ LowercaseChar :/ DigitChar :/ UnderscoreChar :/ QuoteChar :/ CCLNil)
  s =>
  IsSymbol s =>
  SProxy s ->
  PName
pnameP = PName <<< reflectSymbol

pnameToProperName :: forall p. PName -> ProperName p
pnameToProperName (PName s) = ProperName s

pnameToString :: PName -> String
pnameToString (PName s) = s

pnameRegex :: Regex
pnameRegex =
  unsafeRegex "^[A-Z][A-Za-z0-9_']*$" RegexFlags.noFlags

-- iname

newtype IName = IName String

derive newtype instance inameEq :: Eq IName
derive newtype instance inameOrd :: Ord IName

instance inameShow :: Show IName where
  show (IName s) = "(IName " <> show s <> ")"

iname' :: String -> Maybe IName
iname' s =
  IName <$> filterRegex inameRegex s

inameP ::
  forall s.
  NameFormat
  (LowercaseChar :/ UnderscoreChar :/ CCLNil)
  (UppercaseChar :/ LowercaseChar :/ DigitChar :/ UnderscoreChar :/ QuoteChar :/ CCLNil)
  s =>
  IsSymbol s =>
  SProxy s ->
  IName
inameP = IName <<< reflectSymbol

inameRegex :: Regex
inameRegex =
  unsafeRegex "^[a-z_][A-Za-z0-9_]*$" RegexFlags.noFlags

inameToIdent :: IName -> CST.Ident
inameToIdent (IName s) = CST.Ident s

inameToString :: IName -> String
inameToString (IName s) = s

-- opName

newtype OpName = OpName String

derive newtype instance opNameEq :: Eq OpName
derive newtype instance opNameOrd :: Ord OpName

instance opNameShow :: Show OpName where
  show (OpName s) = "(OpName " <> show s <> ")"

unsafeOpName :: String -> OpName
unsafeOpName = OpName

opName' :: String -> Maybe OpName
opName' s =
  guard ( not String.null s
          && all isSymbolChar (toCharArray s)
        )
  $> OpName s

-- currently does not support non ascii symbols
opNameP ::
  forall s.
  NameFormat
  (OperatorChar :/ CCLNil)
  (OperatorChar :/ CCLNil)
  s =>
  IsSymbol s =>
  SProxy s ->
  OpName
opNameP = OpName <<< reflectSymbol

opNameToOpName :: forall p. OpName -> CST.OpName p
opNameToOpName (OpName s) = CST.OpName s

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
  ModuleName
  <$> ( NonEmptyArray.fromArray
        =<< traverse (map pnameToProperName <<< pname') (String.split (String.Pattern ".") s)
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
  ModuleName $ ProperName <$> reflectSList1 (SListProxy :: _ l)

-- qualName

qualNameProper :: String -> Maybe (CST.QualifiedName PName)
qualNameProper = qualName' pname'

qualNameIdent :: String -> Maybe (CST.QualifiedName IName)
qualNameIdent = qualName' iname'

-- TODO support period in opName
qualNameOp :: String -> Maybe (CST.QualifiedName OpName)
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
