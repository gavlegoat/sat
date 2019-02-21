module Transformation (tseitin) where

import Control.Monad.State

import Types

-- Convert a formula to conjunctive normal form.
toCNF :: Formula -> CNFFormula
toCNF (Lit l) = [[l]]
toCNF (And a b) = toCNF a ++ toCNF b
toCNF (Or a b) = [x ++ y | x <- toCNF a, y <- toCNF b]
toCNF (Not (Lit (LNot l))) = [[l]]
toCNF (Not (Lit l)) = [[LNot l]]
toCNF (Not (And a b)) = toCNF (Or (Not a) (Not b))
toCNF (Not (Or a b)) = toCNF (And (Not a) (Not b))
toCNF (Not (Not a)) = toCNF a
toCNF (Not (Implies a b)) = toCNF (And a (Not b))
toCNF (Not (Iff a b)) = toCNF (Not (And (Implies a b) (Implies b a)))
toCNF (Implies a b) = toCNF (Or (Not a) b)
toCNF (Iff a b) = toCNF (And (Implies a b) (Implies b a))

-- Create a conjunction where each clause is a subformula of the original
-- formula labeled with some fresh variable. The return value is the label
-- for the subformula along with the formula representing that subformula.
-- For example, if we start with ((a /\ b) \/ (c /\ d)) /\ e, then we should
-- return the set of clauses:
-- x0 <-> c /\ d
-- x1 <-> a /\ b
-- x2 <-> x1 \/ x0
-- x3 <-> x2 /\ e
-- Note that it is the responsibility of the caller to assert the top-level
-- variable. In the previous example, the caller should add the clause x3 to
-- the returned clause list. LTrue is used in the returned literal to indicate
-- that the returned formula should be used directly. LTrue will only be
-- returned if f is a literal.
tseitinHelp :: Formula -> State Int (Literal, Formula)
tseitinHelp f =
  case f of
    (Lit l) -> return (LTrue, Lit l)
    (Not a) -> do
      (l, f') <- tseitinHelp a
      s <- getFreshVar
      if l == LTrue then return (s, Iff (Lit s) (Not f'))
                    else return (s, Iff (Lit s) (Not (Lit l)))
    (And a b) -> binaryOpTseitin And a b
    (Or a b) -> binaryOpTseitin Or a b
    (Implies a b) -> binaryOpTseitin Implies a b
    (Iff a b) -> binaryOpTseitin Iff a b
  where
    getFreshVar = do
      n <- get
      put (n + 1)
      return $ LVar ("x!" ++ show n)
    binaryOpTseitin op a b = do
      (la, fa) <- tseitinHelp a
      (lb, fb) <- tseitinHelp b
      s <- getFreshVar
      if la == LTrue
         then if lb == LTrue
                 then return (s, Iff (Lit s) (op fa fb))
                 else return (s, Iff (Lit s) (op fa (Lit lb)))
         else if lb == LTrue
                 then return (s, Iff (Lit s) (op (Lit la) fb))
                 else return (s, Iff (Lit s) (op (Lit la) (Lit lb)))

-- Given a formula, produce an equisatisfiable formula in conjunctive normal
-- form.
tseitin :: Formula -> CNFFormula
tseitin f = let (l, f') = evalState (tseitinHelp f) 0
             in [l] : toCNF f'
