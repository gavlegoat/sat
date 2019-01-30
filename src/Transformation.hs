module Transformation (tseitin) where

import qualified Data.Map as Map
import Data.Unique
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Control.Applicative ((<$>))
import Control.Monad

import Types

-- Helper function for binary operations in Tseitin's transformation
mapSubs :: (Formula -> Formula -> Formula) -> Formula -> Formula
        -> IO (Literal, [Formula])
mapSubs op a b = do
  (va, fsa) <- subformulas a
  (vb, fsb) <- subformulas b
  v' <- fmap LVar newUnique
  case (va, vb) of
    (LTrue, LTrue) -> return (v', [Iff (Lit v') (op a b)])
    (LTrue, _    ) -> return (v', Iff (Lit v') (op a (Lit vb)) : fsb)
    (_    , LTrue) -> return (v', Iff (Lit v') (op (Lit va) b) : fsa)
    (_    , _    ) -> return (v', Iff (Lit v') (op (Lit va) (Lit vb)) : fsa ++ fsb)

-- Introduce a new variable for this subformula, recurse and return a list of
-- formulas created in the reursive calls
subformulas :: Formula -> IO (Literal, [Formula])
subformulas (Lit l) = return (LTrue, [Lit l])
subformulas (And a b) = mapSubs And a b
subformulas (Or a b) = mapSubs Or a b
subformulas (Not f) = do
  (v, fs) <- subformulas f
  v' <- fmap LVar newUnique
  if v == LTrue then return (v', [Iff (Lit v') (Not f)])
                else return (v', Iff (Lit v') (Not (Lit v)) : fs)
subformulas (Implies p c) = mapSubs Implies p c
subformulas (Iff a b) = mapSubs Iff a b

-- Convert a formula to CNF, return a list of clauses where each clause is
-- a list of disjuncts
toCNF :: Formula -> IO CNFFormula
toCNF (Lit l) = return [[l]]
toCNF (And a b) = liftM2 (++) (toCNF a) (toCNF b)
toCNF (Or a b) = do
  a' <- toCNF a
  b' <- toCNF b
  return [x ++ y | x <- a', y <- b']
toCNF (Not (Lit l)) = return [[LNot l]]
toCNF (Not (And a b)) = toCNF (Or (Not a) (Not b))
toCNF (Not (Or a b)) = toCNF (And (Not a) (Not b))
toCNF (Not (Not f)) = toCNF f
toCNF (Not (Implies p c)) = toCNF (And p (Not c))
toCNF (Not (Iff a b)) = toCNF (Or (Not (Implies a b)) (Not (Implies b a)))
toCNF (Implies p c) = toCNF (Or (Not p) c)
toCNF (Iff a b) = toCNF (And (Implies a b) (Implies b a))

-- Given a formula, return an equisatisfiable CNF formula
tseitin :: Formula -> IO CNFFormula
tseitin f = do
  fs <- subformulas f
  let fs' = Lit (fst fs) : snd fs
  concat <$> traverse toCNF fs'

clearTrueFalse :: Formula -> Formula
clearTrueFalse (Lit l) = Lit l
clearTrueFalse (And a b) =
  let a' = clearTrueFalse a
      b' = clearTrueFalse b
   in case (a', b') of
        (Lit LFalse, Lit LFalse) -> Lit LFalse
        (Lit LTrue, b'') -> b''
        (a'', Lit LTrue) -> a''
        (a'', b'') -> And a'' b''
clearTrueFalse (Or a b) =
  let a' = clearTrueFalse a
      b' = clearTrueFalse b
   in case (a', b') of
        (Lit LFalse, Lit LFalse) -> Lit LFalse
        (Lit LTrue, _) -> Lit LTrue
        (_, Lit LTrue) -> Lit LTrue
        (a'', b'') -> Or a'' b''
clearTrueFalse (Not f) =
  let f' = clearTrueFalse f
   in case f' of
        Lit LTrue -> Lit LFalse
        Lit LFalse -> Lit LTrue
        _ -> Not f'
clearTrueFalse (Implies a b) =
  let a' = clearTrueFalse a
      b' = clearTrueFalse b
   in case (a', b') of
        (Lit LFalse, _) -> Lit LTrue
        (_, Lit LTrue) -> Lit LTrue
        (Lit LTrue, Lit LFalse) -> Lit LFalse
        (Lit LTrue, b'') -> b''
        (a'', Lit LFalse) -> Not a''
        (a'', b'') -> Implies a'' b''
clearTrueFalse (Iff a b) =
  let a' = clearTrueFalse a
      b' = clearTrueFalse b
   in case (a', b') of
        (Lit LTrue, Lit LTrue) -> Lit LTrue
        (Lit LFalse, Lit LFalse) -> Lit LTrue
        (Lit LTrue, Lit LFalse) -> Lit LFalse
        (Lit LFalse, Lit LTrue) -> Lit LFalse
        (Lit LTrue, b'') -> b''
        (Lit LFalse, b'') -> Not b''
        (a'', Lit LTrue) -> a''
        (a'', Lit LFalse) -> Not a''
        (a'', b'') -> Iff a'' b''
