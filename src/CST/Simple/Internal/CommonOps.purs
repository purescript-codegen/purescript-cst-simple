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
       -- Helpers
       , class RightSingleArrowBindersExpr
       , rightSingleArrowBindersExpr
       ) where

import CST.Simple.Internal.Binder (Binder)
import CST.Simple.Internal.Expr (CaseOfBranch, Expr, caseOfBranch1, caseOfBranchN, exprLambda)
import CST.Simple.Internal.Type (Constraint, Type, typArrow, typConstrained, typKinded)


class RightSingleArrow a b c | a b -> c where
  rightSingleArrow :: a -> b -> c

infixr 6 rightSingleArrow as *->

instance rightSingleArrowType :: RightSingleArrow Type Type Type where
  rightSingleArrow = typArrow

else instance rightSingleArrowCaseOfBranch1 :: RightSingleArrow Binder Expr CaseOfBranch where
  rightSingleArrow = caseOfBranch1

else instance rightSingleArrowBindersExprI ::
  RightSingleArrowBindersExpr a =>
  RightSingleArrow (Array Binder) Expr a where
  rightSingleArrow = rightSingleArrowBindersExpr

--

class LeftSingleArrow a b c | a b -> c where
  leftSingleArrow :: a -> b -> c

infixr 6 leftSingleArrow as *<-

--

class RightDoubleArrow a b c | a b -> c where
  rightDoubleArrow :: a -> b -> c

infixr 10 rightDoubleArrow as *=>

instance rightDoubleArrowType :: RightDoubleArrow Constraint Type Type where
  rightDoubleArrow = typConstrained

--

class DoubleColon a b c | a b -> c where
  doubleColon :: a -> b -> c

infixr 8 doubleColon as *::

instance doubleColonType :: DoubleColon Type String Type where
  doubleColon = typKinded

-- Helpers

class RightSingleArrowBindersExpr a where
  rightSingleArrowBindersExpr :: Array Binder -> Expr -> a

instance rightSingleArrowBindersExprExpr :: RightSingleArrowBindersExpr Expr where
  rightSingleArrowBindersExpr = exprLambda

instance rightSingleArrowBindersExprCaseOfBranch :: RightSingleArrowBindersExpr CaseOfBranch where
  rightSingleArrowBindersExpr = caseOfBranchN
