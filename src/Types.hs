module Types where

import Data.List (intercalate)
import qualified Data.Map as Map

data Literal = LVar String
             | LNot Literal
             | LTrue
             | LFalse
             deriving (Eq)

instance Show Literal where
  show (LVar u) = u
  show (LNot l) = "~" ++ show l
  show LTrue = "True"
  show LFalse = "False"

-- A CNF formula is a conjunction of disjunctions of literals
type CNFFormula = [[Literal]]

data Formula = Lit Literal
             | And Formula Formula
             | Or Formula Formula
             | Not Formula
             | Implies Formula Formula
             | Iff Formula Formula
             deriving (Eq)

instance Show Formula where
  show (Lit l) = show l
  show (And a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (Or a b) = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (Not f) = "~" ++ show f
  show (Implies p c) = "(" ++ show p ++ " -> " ++ show c ++ ")"
  show (Iff a b) = "(" ++ show a ++ " <-> " ++ show b ++ ")"

showClauses :: CNFFormula -> String
showClauses = intercalate " & " . map showClause where
  showClause x = "(" ++ intercalate " | " (map show x) ++ ")"

type Interpretation = Map.Map String Bool

data SATResult = Unsat
               | Sat Interpretation
