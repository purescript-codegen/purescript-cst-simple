module CST.Simple.Names
       ( PName
       , pname'
       , pnameP
       , IName
       , iname'
       , inameP
       , OpName
       , opName'
       , opNameP
       ) where

import Prelude

import CST.Simple.Internal.SymbolUtils (class IsIdentSymbol, class IsOpSymbol, class IsProperSymbol)
import Control.MonadPlus (guard)
import Data.Char.Unicode as Char
import Data.Foldable (all, elem)
import Data.Maybe (Maybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

-- pname

newtype PName = PName String

derive newtype instance pnameEq :: Eq PName
derive newtype instance pnameOrd :: Ord PName

instance pnameShow :: Show PName where
  show (PName s) = "(PName " <> show s <> ")"

pname' :: String -> Maybe PName
pname' s =
  PName <$> filterRegex pnameRegex s

pnameP ::
  forall sym.
  IsProperSymbol sym =>
  IsSymbol sym =>
  SProxy sym ->
  PName
pnameP = PName <<< reflectSymbol

pnameRegex :: Regex
pnameRegex =
  unsafeRegex "^[A-Z][A-Za-z0-9_]*$" RegexFlags.noFlags

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
  forall sym.
  IsIdentSymbol sym =>
  IsSymbol sym =>
  SProxy sym ->
  IName
inameP = IName <<< reflectSymbol

inameRegex :: Regex
inameRegex =
  unsafeRegex "^[a-z][A-Za-z0-9_]*$" RegexFlags.noFlags

-- opName

newtype OpName = OpName String

derive newtype instance opNameEq :: Eq OpName
derive newtype instance opNameOrd :: Ord OpName

instance opNameShow :: Show OpName where
  show (OpName s) = "(OpName " <> show s <> ")"

opName' :: String -> Maybe OpName
opName' s =
  guard ( not String.null s
          && all isSymbolChar (toCharArray s)
        )
  $> OpName s

-- currently does not support non ascii symbols
opNameP ::
  forall sym.
  IsOpSymbol sym =>
  IsSymbol sym =>
  SProxy sym ->
  OpName
opNameP = OpName <<< reflectSymbol

isSymbolChar :: Char -> Boolean
isSymbolChar c =
  -- https://github.com/purescript/purescript/blob/48634d05da1021ac1bcb4907461ae9b2d3b0e699/lib/purescript-cst/src/Language/PureScript/CST/Lexer.hs#L659
  c `elem` asciiSymbolChars
  || (not (Char.isAscii c) && Char.isSymbol c)

asciiSymbolChars :: Array Char
asciiSymbolChars = toCharArray ":!#$%&*+./<=>?@\\^|-~"

--

filterRegex :: Regex -> String -> Maybe String
filterRegex re s =
  guard (Regex.test re s) $> s
