module Types where

import Data.Unique
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Map as Map

type VarMap = Map.Map Unique String

data Literal = LTrue
             | LFalse
             | LVar Unique
             | LNot Literal
             deriving (Eq)

showLit :: VarMap -> Literal -> String
showLit _ LTrue = "true"
showLit _ LFalse = "false"
showLit m (LVar u) = fromMaybe ("x" ++ show (hashUnique u)) (Map.lookup u m)
showLit m (LNot l) = "~" ++ showLit m l

-- A CNF formula is a conjunction of disjunctions of literals
type CNFFormula = [[Literal]]

data Formula = Lit Literal
             | And Formula Formula
             | Or Formula Formula
             | Not Formula
             | Implies Formula Formula
             | Iff Formula Formula
             deriving (Eq)

showFormula :: VarMap -> Formula -> String
showFormula m (Lit l) = showLit m l
showFormula m (And a b) = "(" ++ showFormula m a ++ " & " ++ showFormula m b ++ ")"
showFormula m (Or a b) = "(" ++ showFormula m a ++ " | " ++ showFormula m b ++ ")"
showFormula m (Not f) = "~" ++ showFormula m f
showFormula m (Implies p c) = "(" ++ showFormula m p ++ " -> " ++ showFormula m c ++ ")"
showFormula m (Iff a b) = "(" ++ showFormula m a ++ " <-> " ++ showFormula m b ++ ")"

showClauses :: VarMap -> CNFFormula -> String
showClauses m = intercalate " & " . map (showClause m) where
  showClause m x = "(" ++ intercalate " | " (map (showLit m) x) ++ ")"
