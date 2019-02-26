module CDCL (checkSAT) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Graph.Inductive as Graph
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

