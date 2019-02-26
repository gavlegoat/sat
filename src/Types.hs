module Types ( Literal(..)
             , CNFFormula
             , cnfToList
             , cnfFromList
             , simplifyCNF
             , assignVar
             , cnfHasNoClauses
             , cnfHasEmptyClause
             , Formula(..)
             , Interpretation(..)
             , interpFromMap
             , emptyInterp
             , addBinding
             , restrictInterpretation
             , SATResult(..)
             ) where

import Data.List (intercalate, notElem)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Literal = LVar String
             | LNot Literal
             | LTrue
             | LFalse
             deriving (Eq)

instance Show Literal where
  show (LVar u) = u
  show (LNot l) = "~" ++ show l
  show LTrue    = "True"
  show LFalse   = "False"

-- A CNF formula is a conjunction of disjunctions of literals
newtype CNFFormula = CNFFormula { cnfToList :: [[Literal]] }

instance Show CNFFormula where
  show = intercalate " & " . map showClause . cnfToList where
    showClause x = "(" ++ intercalate " | " (map show x) ++ ")"

cnfFromList :: [[Literal]] -> CNFFormula
cnfFromList = CNFFormula

-- Remove LTrue and LFalse from a CNF formula
simplifyCNF :: CNFFormula -> CNFFormula
simplifyCNF = cnfFromList . removeLTrue . removeLFalse . removeNot . cnfToList
  where
    -- Deal with double negations or negations of false and true
    removeNot = map (map processLiteral)
    processLiteral (LNot (LNot l)) = processLiteral l
    processLiteral (LNot LTrue) = LFalse
    processLiteral (LNot LFalse) = LTrue
    processLiteral l = l
    -- If a literal is false we can remove that literal
    removeLFalse = map (filter (/= LFalse))
    -- If a literal is true we can remove the containing clause
    removeLTrue = filter (notElem LTrue)

assignVar :: String -> Bool -> CNFFormula -> CNFFormula
assignVar v b = simplifyCNF . cnfFromList . map processClause . cnfToList where
  processClause = map processLiteral
  processLiteral l = case l of
                       LNot l' -> LNot $ processLiteral l'
                       LVar u | u == v -> if b then LTrue else LFalse
                       lit -> lit

cnfHasNoClauses :: CNFFormula -> Bool
cnfHasNoClauses = null . cnfToList

cnfHasEmptyClause :: CNFFormula -> Bool
cnfHasEmptyClause = any null . cnfToList

data Formula = Lit Literal
             | And Formula Formula
             | Or Formula Formula
             | Not Formula
             | Implies Formula Formula
             | Iff Formula Formula
             deriving (Eq)

instance Show Formula where
  show (Lit l)       = show l
  show (Not f)       = "~" ++ show f
  show (And a b)     = "(" ++ show a ++ " & "   ++ show b ++ ")"
  show (Or a b)      = "(" ++ show a ++ " | "   ++ show b ++ ")"
  show (Implies p c) = "(" ++ show p ++ " -> "  ++ show c ++ ")"
  show (Iff a b)     = "(" ++ show a ++ " <-> " ++ show b ++ ")"

newtype Interpretation = Interpretation { interpToMap :: Map.Map String Bool }

instance Show Interpretation where
  show i = "[" ++ showBindings i ++ "]" where
    showBindings =
      intercalate ", " .
        map (\(v, b) -> v ++ " -> " ++ show b) .
          Map.assocs . interpToMap

emptyInterp :: Interpretation
emptyInterp = Interpretation Map.empty

interpFromMap :: Map.Map String Bool -> Interpretation
interpFromMap = Interpretation

addBinding :: String -> Bool -> Interpretation -> Interpretation
addBinding s b = interpFromMap . Map.insert s b . interpToMap

restrictInterpretation :: Set.Set String -> Interpretation -> Interpretation
restrictInterpretation vs =
  interpFromMap . flip Map.restrictKeys vs . interpToMap

data SATResult = Unsat
               | Sat Interpretation

instance Show SATResult where
  show Unsat = "UNSAT"
  show (Sat i) = "SAT\n" ++ show i
