module CST.Simple.Internal.CommonWords
       ( LetBindingsB
       , _let
       , _do
       , _letd
       , AdoStatements
       , _ado
       , class InOp
       , _in
       , _where
       ) where

import CST.Simple.Internal.Expression (DoStatement, Expr, LetBinding, Where, doLet, exprAdo, exprDo, exprLetIn, whr)

newtype LetBindingsB = LetBindingsB (Array LetBinding)

_let :: Array LetBinding -> LetBindingsB
_let = LetBindingsB

_letd :: Array LetBinding -> DoStatement
_letd = doLet

class InOp a b c | a -> b c where
  _in :: a -> b -> c

instance inOpLet :: InOp LetBindingsB Expr Expr where
  _in (LetBindingsB ls) = exprLetIn ls

else instance inOpAdo :: InOp AdoStatements Expr Expr where
  _in (AdoStatements ds) = exprAdo ds

_do :: Array DoStatement -> Expr
_do = exprDo

newtype AdoStatements = AdoStatements (Array DoStatement)

_ado :: Array DoStatement -> AdoStatements
_ado = AdoStatements

_where :: Expr -> Array LetBinding -> Where
_where = whr
