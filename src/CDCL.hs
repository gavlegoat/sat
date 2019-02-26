module CDCL (checkSAT) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Graph.Inductive as Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State

import Types
import Transformation

-- For CDCL it will be convenient to have names for the clauses
type CDCLFormula = Map Int [Literal]

cnfToCDCL :: CNFFormula -> State Int CDCLFormula
cnfToCDCL = cnfToCDCL' Map.empty . cnfToList where
  cnfToCDCL' m []       = return m
  cnfToCDCL' m (l : ls) = do
    n <- get
    put (n + 1)
    cnfToCDCL' (Map.insert n l m) ls

-- An assignment consists of a variable, a value it's assigned to, and a
-- decision level
type Assignment = (String, Bool, Int)

-- A clause name
type Clause = String

-- An implication graph labels nodes with assignments and edges with the
-- clauses from which those assignments can be derived
type ImplicationGraph = Graph.Gr Assignment Clause

data CDCLState =
  CDCLState { implGraph :: ImplicationGraph
            , currentAssignment :: Interpretation
            , decisionLevel :: Int
            -- The set of variables not assigned in currentAssignment
            , unassigned :: Set String
            -- The set of variables where we have tried assigning True in a
            -- decide step. Variables assigned to True in BCP should not be
            -- put into triedTrue
            , triedTrue :: Set String
            }

assign :: CDCLFormula -> String -> Bool -> State CDCLState CDCLFormula
assign = undefined

pureLiteralProp :: CDCLFormula -> State CDCLState CDCLFormula
pureLiteralProp = undefined

-- Perform BCP and return true if there is a conflict
bcp :: CDCLFormula -> State CDCLState Bool
bcp = undefined

-- If all variables are assigned, return sat
-- otherwise, pick a branching variable v
-- increment the decision level
-- add an assignment for v
-- if bcp leads to a conflict:
--   let (c, d') be a conflict clause and decision level
--   if d' < 0:
--     return unsat
--   backtrack to level d'
--   add c
--   set the decision level to d'
-- else:
--   cdcl with the new assignment added
cdclLoop :: CDCLFormula -> State CDCLState SATResult
cdclLoop f = do
  s <- get
  if Set.null (unassigned s)
     then return $ Sat $ currentAssignment s
     else do
       let v = Set.findMin $ unassigned s
       let b = not (Set.member v $ triedTrue s)
       assign f v b
       put $ s { triedTrue = Set.insert v (triedTrue s) }
       conflict <- bcp f
       if conflict
          then undefined
          else cdclLoop f

-- if bcp f leads to a conflict, return unsat, otherwise return cdclLoop
cdcl :: CDCLFormula -> State ImplicationGraph SATResult
cdcl = undefined

checkSAT :: Formula -> SATResult
checkSAT = undefined
