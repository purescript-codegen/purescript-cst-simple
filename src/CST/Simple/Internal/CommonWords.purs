module CST.Simple.Internal.CommonWords
       ( _do
       , AdoStatements
       , _ado
       , _in
       , _where
       ) where

import CST.Simple.Internal.Expr (DoStatement, Expr, LetBinding, Where, exprAdo, exprDo, whr)

_do :: Array DoStatement -> Expr
_do = exprDo

newtype AdoStatements = AdoStatements (Array DoStatement)

_ado :: Array DoStatement -> AdoStatements
_ado = AdoStatements

_in :: AdoStatements -> Expr -> Expr
_in (AdoStatements ds) = exprAdo ds

_where :: Expr -> Array LetBinding -> Where
_where = whr
