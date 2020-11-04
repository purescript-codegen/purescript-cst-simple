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
       , class Equals
       , equals
       , (*=)
       , class Space
       , space
       , (*-)
       -- Helpers
       , class RightSingleArrowBindersExpr
       , rightSingleArrowBindersExpr
       ) where

import CST.Simple.Internal.Binder (Binder)
import CST.Simple.Internal.Expr (CaseOfBranch, Expr, LetBinding, caseOfBranch1, caseOfBranchN, exprLambda, grd_, letName, letPattern, letSig, whr_)
import CST.Simple.Internal.NamedBinders (NamedBinders(..), namedBinders1, nbAddBinder)
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

instance doubleColonLetBinding :: DoubleColon String Type LetBinding where
  doubleColon = letSig

--

class Equals a b c | a b -> c where
  equals :: a -> b -> c

infixr 6 equals as *=

instance equalsLetBinding :: Equals String Expr LetBinding where
  equals s e = letName s [] (grd_ e)

instance equalsLetBinding' :: Equals NamedBinders Expr LetBinding where
  equals (NamedBinders s bs) e = letName s bs (grd_ e)

instance equalsLetBindingPattern :: Equals Binder Expr LetBinding where
  equals b e = letPattern b (whr_ e)

--

class Space a b c | a b -> c where
  space :: a -> b -> c

infixl 7 space as *-

instance spaceNamedBinders :: Space String Binder NamedBinders where
  space = namedBinders1

instance spaceNamedBinders1 :: Space NamedBinders Binder NamedBinders where
  space = nbAddBinder

-- Helpers

class RightSingleArrowBindersExpr a where
  rightSingleArrowBindersExpr :: Array Binder -> Expr -> a

instance rightSingleArrowBindersExprExpr :: RightSingleArrowBindersExpr Expr where
  rightSingleArrowBindersExpr = exprLambda

instance rightSingleArrowBindersExprCaseOfBranch :: RightSingleArrowBindersExpr CaseOfBranch where
  rightSingleArrowBindersExpr = caseOfBranchN
