module CST.Simple.Internal.CommonOp
       ( class RightSingleArrow
       , rightSingleArrow
       , (*->)
       , class LeftSingleArrow
       , leftSingleArrow
       , (*<-)
       , class RightDoubleArrow
       , rightDoubleArrow
       , (*=>)
       , class DoubleColon
       , doubleColon
       , (*::)
       ) where

import CST.Simple.Internal.Binder (Binder)
import CST.Simple.Internal.Expr (Expr, exprLambda)
import CST.Simple.Internal.Type (Constraint, Typ, typArrow, typConstrained, typKinded)

class RightSingleArrow a b c | a b -> c, c -> a b where
  rightSingleArrow :: a -> b -> c

infixr 6 rightSingleArrow as *->

instance rightSingleArrowType :: RightSingleArrow Typ Typ Typ where
  rightSingleArrow = typArrow

instance rightSingleArrowExpr :: RightSingleArrow (Array Binder) Expr Expr where
  rightSingleArrow = exprLambda

--

class LeftSingleArrow a b c | a b -> c, c -> a b where
  leftSingleArrow :: a -> b -> c

infixr 6 leftSingleArrow as *<-

--

class RightDoubleArrow a b c | a b -> c, c -> a b where
  rightDoubleArrow :: a -> b -> c

infixr 10 rightDoubleArrow as *=>

instance rightDoubleArrowType :: RightDoubleArrow Constraint Typ Typ where
  rightDoubleArrow = typConstrained

--

class DoubleColon a b c | a b -> c, c -> a b where
  doubleColon :: a -> b -> c

infixr 8 doubleColon as *::

instance doubleColonType :: DoubleColon Typ String Typ where
  doubleColon = typKinded
