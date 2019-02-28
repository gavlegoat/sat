module CDCL (checkSAT) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Graph.Inductive as Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (foldM, sequence)

import Types
import Transformation

-- A clauses consists of a set of literals
type Clause = [Literal]

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []       = Nothing
safeIndex 0 (l : _ ) = Just l
safeIndex n (_ : ls) = safeIndex (n - 1) ls

-- A clause number
type ClauseLabel = Int

-- For CDCL it will be convenient to have names for the clauses
type CDCLFormula = Map ClauseLabel Clause

cnfToCDCL :: CNFFormula -> State Int CDCLFormula
cnfToCDCL = cnfToCDCL' Map.empty . cnfToList . simplifyCNF where
  cnfToCDCL' m []       = return m
  cnfToCDCL' m (l : ls) = do
    n <- get
    put (n + 1)
    if null l
       then cnfToCDCL' m ls
       else cnfToCDCL' (Map.insert n l m) ls

-- An assignment consists of a variable, a value it's assigned to, and a
-- decision level
data ImplNode = Assignment String Bool Int
              | Conflict

-- An implication graph labels nodes with assignments and edges with the
-- clauses from which those assignments can be derived
type ImplicationGraph = Graph.Gr ImplNode ClauseLabel

data CDCLState =
  CDCLState { implGraph :: ImplicationGraph
            -- The graph has integers as nodes but we want to look up nodes
            -- by the name of the assigned variable. This is constant.
            , varToNode :: Map String Graph.Node
            , currentAssignment :: Interpretation
            , decisionLevel :: Int
            -- The set of variables not assigned in currentAssignment
            , unassigned :: Set String
            -- The set of variables where we have tried assigning True in a
            -- decide step. Variables assigned to True in BCP should not be
            -- put into triedTrue
            , triedTrue :: Set String
            -- The next available number for labelling a clause
            , nextClauseNumber :: Int
            -- The watch literals for each clause. If the clause has no
            -- unassigned variables then it is mapped to Nothing.
            , watchLits :: Map ClauseLabel (Maybe (Literal, Literal))
            }

-- Assign a variable to a value
assign :: String -> Bool -> State CDCLState ()
assign v b = do
  s <- get
  let ig = implGraph s
  let d = decisionLevel s
  let n = head $ Graph.newNodes 1 ig
  put $ s {         implGraph = Graph.insNode (n, Assignment v b d) ig
          ,         varToNode = Map.insert v n $ varToNode s
          , currentAssignment = addBinding v b $ currentAssignment s
          ,        unassigned = Set.delete v $ unassigned s }

data BCPResult = BCPConflict
               | BCPAssign String
               | BCPNothing
               deriving (Eq)

-- Perform BCP and return true if there is a conflict. Takes as input a set of
-- variables which need to be processed (i.e., newly assigned variables).
-- For each variable v we need to look at:
--   For each clause c:
--     If v is one of c's watch literals:
--       we need to see if c is now a unit clause. If so, add something to the
--           implication graph, add the newly set variable to vs
--       If c is not a unit clause, find a new unassigned watch literal
-- If we added anything to v we need to recurse
bcp :: CDCLFormula -> Set String -> State CDCLState Bool
bcp f vs = do
  vs' <- foldM processVar (Just Set.empty) (Set.toList vs)
  case vs' of
    Nothing -> return True
    Just s  -> if Set.null s then return False else bcp f s
 where
   -- processVar will return Nothing if there is a conflict and otherwise a set
   -- of variables which were set during BCP
   processVar Nothing   _ = return Nothing
   processVar (Just vs) v = do
     vs' <- mapM processClause (Map.assocs f)
     if BCPConflict `elem` vs' then return Nothing
                               else return . Just $ extractAssigns vs'
   processClause :: (ClauseLabel, Clause) -> State CDCLState BCPResult
   processClause = undefined
   extractAssigns =
     Set.fromList . map (\(BCPAssign s) -> s) . filter (/= BCPNothing)

-- Given the current assignment, analyze the conflict to derive a conflict
-- clause and return it along with the decision level we need to backtrack to.
analyzeConflict :: CDCLFormula -> State CDCLState ([Literal], Int)
analyzeConflict f = undefined

-- Backtrack to the given decision level, undoing all of the bindings at
-- greater decision levels.
backtrack :: CDCLFormula -> Int -> State CDCLState ()
backtrack f n = undefined

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
       assign v b
       put $ s { triedTrue = Set.insert v (triedTrue s) }
       conflict <- bcp f $ Set.singleton v
       if conflict
          then do
            (cc, d') <- analyzeConflict f
            if d' < 0
               then return Unsat
               else do
                 backtrack f d'
                 s <- get
                 put $ s {    decisionLevel = d'
                         , nextClauseNumber = nextClauseNumber s + 1 }
                 cdclLoop $ Map.insert (nextClauseNumber s) cc f
          else cdclLoop f

-- This BCP takes care of singleton clauses in the initial formula. After this
-- call, each clause in the formula should consist of at least two variables.
-- Note that this is the only function which modifies the formula. Each other
-- function only interacts with the fields of CDCLState.
initialBCP :: CDCLFormula -> (CDCLFormula, Interpretation)
initialBCP = undefined

-- if bcp f leads to a conflict, return unsat, otherwise return cdclLoop
cdcl :: CDCLFormula -> State ImplicationGraph SATResult
cdcl = undefined

checkSAT :: Formula -> SATResult
checkSAT = undefined
