module CST.Simple.Internal.SymbolUtils
       ( kind CharClass
       , UppercaseChar
       , LowercaseChar
       , DigitChar
       , UnderscoreChar
       , OperatorChar
       , class HasCharClass
       , class IsWordCharClass
       , class IsProperSymbol
       , class IsIdentSymbol
       , class IsWordTail
       , class IsOpSymbol
       , class IsOpTail
       ) where

import Prim.Symbol as Symbol

foreign import kind CharClass
foreign import data UppercaseChar :: CharClass
foreign import data LowercaseChar :: CharClass
foreign import data DigitChar :: CharClass
foreign import data UnderscoreChar :: CharClass
foreign import data OperatorChar :: CharClass

class HasCharClass (s :: Symbol) (c :: CharClass) | s -> c

instance hasCharClass_A :: HasCharClass "A" UppercaseChar
instance hasCharClass_B :: HasCharClass "B" UppercaseChar
instance hasCharClass_C :: HasCharClass "C" UppercaseChar
instance hasCharClass_D :: HasCharClass "D" UppercaseChar
instance hasCharClass_E :: HasCharClass "E" UppercaseChar
instance hasCharClass_F :: HasCharClass "F" UppercaseChar
instance hasCharClass_G :: HasCharClass "G" UppercaseChar
instance hasCharClass_H :: HasCharClass "H" UppercaseChar
instance hasCharClass_I :: HasCharClass "I" UppercaseChar
instance hasCharClass_J :: HasCharClass "J" UppercaseChar
instance hasCharClass_K :: HasCharClass "K" UppercaseChar
instance hasCharClass_L :: HasCharClass "L" UppercaseChar
instance hasCharClass_M :: HasCharClass "M" UppercaseChar
instance hasCharClass_N :: HasCharClass "N" UppercaseChar
instance hasCharClass_O :: HasCharClass "O" UppercaseChar
instance hasCharClass_P :: HasCharClass "P" UppercaseChar
instance hasCharClass_Q :: HasCharClass "Q" UppercaseChar
instance hasCharClass_R :: HasCharClass "R" UppercaseChar
instance hasCharClass_S :: HasCharClass "S" UppercaseChar
instance hasCharClass_T :: HasCharClass "T" UppercaseChar
instance hasCharClass_U :: HasCharClass "U" UppercaseChar
instance hasCharClass_V :: HasCharClass "V" UppercaseChar
instance hasCharClass_W :: HasCharClass "W" UppercaseChar
instance hasCharClass_X :: HasCharClass "X" UppercaseChar
instance hasCharClass_Y :: HasCharClass "Y" UppercaseChar
instance hasCharClass_Z :: HasCharClass "Z" UppercaseChar

instance hasCharClass_a :: HasCharClass "a" LowercaseChar
instance hasCharClass_b :: HasCharClass "b" LowercaseChar
instance hasCharClass_c :: HasCharClass "c" LowercaseChar
instance hasCharClass_d :: HasCharClass "d" LowercaseChar
instance hasCharClass_e :: HasCharClass "e" LowercaseChar
instance hasCharClass_f :: HasCharClass "f" LowercaseChar
instance hasCharClass_g :: HasCharClass "g" LowercaseChar
instance hasCharClass_h :: HasCharClass "h" LowercaseChar
instance hasCharClass_i :: HasCharClass "i" LowercaseChar
instance hasCharClass_j :: HasCharClass "j" LowercaseChar
instance hasCharClass_k :: HasCharClass "k" LowercaseChar
instance hasCharClass_l :: HasCharClass "l" LowercaseChar
instance hasCharClass_m :: HasCharClass "m" LowercaseChar
instance hasCharClass_n :: HasCharClass "n" LowercaseChar
instance hasCharClass_o :: HasCharClass "o" LowercaseChar
instance hasCharClass_p :: HasCharClass "p" LowercaseChar
instance hasCharClass_q :: HasCharClass "q" LowercaseChar
instance hasCharClass_r :: HasCharClass "r" LowercaseChar
instance hasCharClass_s :: HasCharClass "s" LowercaseChar
instance hasCharClass_t :: HasCharClass "t" LowercaseChar
instance hasCharClass_u :: HasCharClass "u" LowercaseChar
instance hasCharClass_v :: HasCharClass "v" LowercaseChar
instance hasCharClass_w :: HasCharClass "w" LowercaseChar
instance hasCharClass_x :: HasCharClass "x" LowercaseChar
instance hasCharClass_y :: HasCharClass "y" LowercaseChar
instance hasCharClass_z :: HasCharClass "z" LowercaseChar

instance hasCharClass_0 :: HasCharClass "0" DigitChar
instance hasCharClass_1 :: HasCharClass "1" DigitChar
instance hasCharClass_2 :: HasCharClass "2" DigitChar
instance hasCharClass_3 :: HasCharClass "3" DigitChar
instance hasCharClass_4 :: HasCharClass "4" DigitChar
instance hasCharClass_5 :: HasCharClass "5" DigitChar
instance hasCharClass_6 :: HasCharClass "6" DigitChar
instance hasCharClass_7 :: HasCharClass "7" DigitChar
instance hasCharClass_8 :: HasCharClass "8" DigitChar
instance hasCharClass_9 :: HasCharClass "9" DigitChar

instance hasCharClass_Underscore :: HasCharClass "_" UnderscoreChar

instance hasCharClass_OpColon :: HasCharClass ":" OperatorChar
instance hasCharClass_OpExcl :: HasCharClass "!" OperatorChar
instance hasCharClass_OpHash :: HasCharClass "#" OperatorChar
instance hasCharClass_OpDollar :: HasCharClass "$" OperatorChar
instance hasCharClass_OpPct :: HasCharClass "%" OperatorChar
instance hasCharClass_OpAmp :: HasCharClass "&" OperatorChar
instance hasCharClass_OpAst :: HasCharClass "*" OperatorChar
instance hasCharClass_OpPlus :: HasCharClass "+" OperatorChar
instance hasCharClass_OpPeriod :: HasCharClass "." OperatorChar
instance hasCharClass_OpSlash :: HasCharClass "/" OperatorChar
instance hasCharClass_OpLt :: HasCharClass "<" OperatorChar
instance hasCharClass_OpEq :: HasCharClass "=" OperatorChar
instance hasCharClass_OpGt :: HasCharClass ">" OperatorChar
instance hasCharClass_OpQ :: HasCharClass "?" OperatorChar
instance hasCharClass_OpAt :: HasCharClass "@" OperatorChar
instance hasCharClass_OpBSlash :: HasCharClass "\\" OperatorChar
instance hasCharClass_OpCaret :: HasCharClass "^" OperatorChar
instance hasCharClass_OpPipe :: HasCharClass "|" OperatorChar
instance hasCharClass_OpHyphen :: HasCharClass "-" OperatorChar
instance hasCharClass_OpTilde :: HasCharClass "~" OperatorChar
instance hasCharClass_OpDQuotes :: HasCharClass "\"" OperatorChar

class IsWordCharClass (c :: CharClass)

instance isWordCharClassUppercase :: IsWordCharClass UppercaseChar
instance isWordCharClassLowercase :: IsWordCharClass LowercaseChar
instance isWordCharClassDigit :: IsWordCharClass DigitChar
instance isWordCharClassUnderscore :: IsWordCharClass UnderscoreChar

class IsProperSymbol (s :: Symbol)

instance isProperSymbolI ::
  ( HasCharClass h UppercaseChar
  , IsWordTail t
  , Symbol.Cons h t sym
  ) =>
  IsProperSymbol sym

class IsIdentSymbol (s :: Symbol)

instance isIdentSymbolI ::
  ( HasCharClass h LowercaseChar
  , IsWordTail t
  , Symbol.Cons h t sym
  ) =>
  IsIdentSymbol sym

class IsWordTail (s :: Symbol)

instance isWordTailNil :: IsWordTail ""
else instance isWordTailCons ::
  ( HasCharClass h c
  , IsWordCharClass c
  , IsWordTail t
  , Symbol.Cons h t sym
  ) => IsWordTail sym

class IsOpSymbol (s :: Symbol)

instance isOpSymbolI ::
  ( HasCharClass h OperatorChar
  , IsOpTail t
  , Symbol.Cons h t sym
  ) =>
  IsOpSymbol sym

class IsOpTail (s :: Symbol)

instance isOpTailNil :: IsOpTail ""
else instance isOpTailCons ::
  ( HasCharClass h OperatorChar
  , IsOpTail t
  , Symbol.Cons h t sym
  ) => IsOpTail sym
