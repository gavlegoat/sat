module CDCL (checkSAT) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Graph.Inductive as Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (foldM, foldM_, sequence)

import Types
import Transformation

{-# ANN module "HLint: ignore Reduce duplication" #-}

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

-- Assign a variable to a value. Return true if there is a conflict.
assign :: String -> Bool -> State CDCLState Bool
assign v b = do
  s <- get
  let ig = implGraph s
  let d = decisionLevel s
  let n = head $ Graph.newNodes 1 ig
  case Map.lookup v . interpToMap $ currentAssignment s of
    Just b' | b' /= b -> return True
    _ -> do
      put $ s {         implGraph = Graph.insNode (n, Assignment v b d) ig
              ,         varToNode = Map.insert v n $ varToNode s
              , currentAssignment = addBinding v b $ currentAssignment s
              ,        unassigned = Set.delete v $ unassigned s }
      return False

-- Add an edge to the implication graph from the variable in the given
-- literal to the given node, labeled with the given clause. Note that
-- if the given literal is for the variable in the node we should avoid
-- adding a self-edge
addEdge :: String -> Graph.Node -> ClauseLabel -> Literal -> State CDCLState ()
addEdge var n i l = do
  let v = case l of
            LVar u -> u
            LNot (LVar u) -> u
  if v == var then return () else do
    s <- get
    case Map.lookup v $ varToNode s of
      Nothing -> error "An assigned node is not in the implication graph"
      Just n' -> put $ s { implGraph = Graph.insEdge (n', n, i) $ implGraph s }

-- Perform an assignment which is caused by some clause. This implies that the
-- new node in the implication graph will need incoming edges.
assignImpl :: ClauseLabel -> Clause -> String -> Bool -> State CDCLState Bool
assignImpl i c v b = do
  s <- get
  let ig = implGraph s
  let d = decisionLevel s
  let n = head $ Graph.newNodes 1 ig
  case Map.lookup v . interpToMap $ currentAssignment s of
    Just b' | b' /= b -> return True
    _ -> do
      let ng = Graph.insNode (n, Assignment v b d) ig
      mapM_ (addEdge v n i) c
      return False

data BCPResult = BCPConflict
               | BCPAssign String
               | BCPNothing
               deriving (Eq)

-- This function is called whenever a watch literal for some clause is
-- assigned. We need to check to see if this clause becomes a unit. If it
-- does, we should assign the remaining literal and return it. If this
-- assignment is inconsistent, then we should return BCPConflict. Otherwise,
-- we should assign new watch literals and return BCPNothing.
checkWatchLiterals :: ClauseLabel -> Clause -> State CDCLState BCPResult
checkWatchLiterals i c = do
  vars <- findUnassigned c
  if Set.size vars == 1
     then do
       let v = Set.elemAt 0 vars
       conf <- assignImpl i c v $ appearsPositive v c
       if conf
          then do
            s <- get
            let ig = implGraph s
            let d = decisionLevel s
            let n = head $ Graph.newNodes 1 ig
            let ng = Graph.insNode (n, Conflict) ig
            mapM_ (addEdge "" n i) c
            return BCPConflict
          else return $ BCPAssign v
     else do
       let v1 = findLiteral (Set.elemAt 0 vars) c
       let v2 = findLiteral (Set.elemAt 1 vars) c
       s <- get
       put $ s { watchLits = Map.insert i (Just (v1, v2)) $ watchLits s }
       return BCPNothing
 where
   findUnassigned = foldM processLiteral Set.empty
   processLiteral :: Set String -> Literal -> State CDCLState (Set String)
   processLiteral vs l = do
     let v = case l of
               LVar u -> u
               LNot (LVar u) -> u
     s <- get
     case Map.lookup v . interpToMap $ currentAssignment s of
       Nothing -> return $ Set.insert v vs
       Just _ -> return vs
   findLiteral v [] = error "Variable not found in a clause where it should be"
   findLiteral v (l : ls) = case l of
                              LVar u | u == v -> l
                              LNot (LVar u) | u == v -> l
                              _ -> findLiteral v ls
   appearsPositive v c = case findLiteral v c of
                           LVar _ -> True
                           _ -> False

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
     vs' <- mapM (processClause v) (Map.assocs f)
     if BCPConflict `elem` vs' then return Nothing
                               else return . Just $ extractAssigns vs'
   processClause v (i, c) = do
     s <- get
     case Map.lookup i $ watchLits s of
       Nothing -> error "Clause with no watch literals found"
       Just Nothing -> return BCPNothing  -- This clause is already satisfied
       Just (Just (LVar l1, LVar l2))
         | v == l1 -> checkWatchLiterals i c
         | v == l2 -> checkWatchLiterals i c
       Just (Just (LVar l1, LNot (LVar l2)))
         | v == l1 -> checkWatchLiterals i c
         | v == l2 -> checkWatchLiterals i c
       Just (Just (LNot (LVar l1), LVar l2))
         | v == l1 -> checkWatchLiterals i c
         | v == l2 -> checkWatchLiterals i c
       Just (Just (LNot (LVar l1), LNot (LVar l2)))
         | v == l1 -> checkWatchLiterals i c
         | v == l2 -> checkWatchLiterals i c
       _ -> return BCPNothing   -- v is neither of c's watch literals
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
       _ <- assign v b   -- There can't be a conflict because v is unassigned
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
