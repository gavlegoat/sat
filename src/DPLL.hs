module DPLL (checkSAT) where

import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Types
import Transformation

-- Given a literal and an assignment, replace all instances of that literal
-- with a constant.
literalProp :: CNFFormula -> Literal -> Bool -> CNFFormula
literalProp f l b =
  simplifyCNF . cnfFromList $ replaceWithTF (cnfToList f) l b where
    replaceWithTF f l b = map (map (\x -> if x == l
                                             then if b
                                                     then LTrue
                                                     else LFalse
                                             else x)) f

-- Collect the set of variables used in the input formula
collectVars :: CNFFormula -> Set String
collectVars =
  foldl' (\a x -> Set.union a $ processClause x) Set.empty . cnfToList where
    processClause = foldl' (\a x -> case extractString x of
                                      Nothing -> a
                                      Just s -> Set.insert s a) Set.empty
    extractString x = case x of
                        LTrue  -> Nothing
                        LFalse -> Nothing
                        LVar s -> Just s
                        LNot l -> extractString l

type DPLLState = (CNFFormula, Set String, Interpretation)

-- Assign a variable to a value and return the resulting formula, the variable
-- set without the assigned variable, and an interpretation with the new
-- assignment
assign :: String -> Bool -> DPLLState -> Maybe DPLLState
assign v b (cs, vs, i) =
  let cs' = assignVar v b cs
   in if cnfHasEmptyClause cs'  -- empty clause -> unsatisfiable
         then Nothing
         else Just (cs', Set.delete v vs, addBinding v b i)

-- Given a symbol s, if the Maybe Bool is Nothing, do not chainge the formula,
-- otherwise if the Maybe Bool is Just b assign s to b in the interpretation
-- and remove all clauses containing s from the formula
propogateMaybePureLiteral :: String -> Maybe Bool -> DPLLState -> DPLLState
-- If the symbol is not pure, we don't need to change anything in the state
propogateMaybePureLiteral _ Nothing s = s
propogateMaybePureLiteral v (Just b) (f, vs, i) =
  (cnfFromList . filter (notContains v) $ cnfToList f,
   Set.delete v vs, addBinding v b i) where
     -- Determine whether v appears in the given clause
     notContains :: String -> [Literal] -> Bool
     notContains _ [] = True
     notContains v (LVar u : ls) | u == v = False
     notContains v (LNot (LVar u) : ls) | u == v = False
     notContains v (_ : ls) = notContains v ls

-- Look for pure literals to assign
pureLiteralProp :: DPLLState -> Maybe DPLLState
pureLiteralProp (f, vs, i) =
  let pureMap = foldl' (foldl' processLiteral) Map.empty $ cnfToList f
   in if Map.filter isJust pureMap == Map.empty
         then if cnfHasEmptyClause f then Nothing else Just (f, vs, i)
         -- Removing clauses may cause new literals to become pure, so we need
         -- to call pureLiteralProp again on the result
         else pureLiteralProp $ Map.foldrWithKey propogateMaybePureLiteral
                                                 (f, vs, i) pureMap
 where
   processLiteral m (LVar s) =
     case Map.lookup s m of
       -- This literal has not been seen -> only seen positive
       Nothing -> Map.insert s (Just True) m
       -- This literal was already seen negative -> seen both positive and negative
       Just (Just False) -> Map.insert s Nothing m
       -- In any other case we don't need to change anything.
       _ -> m
   processLiteral m (LNot (LVar s)) =
     case Map.lookup s m of
       Nothing -> Map.insert s (Just False) m
       Just (Just True) -> Map.insert s Nothing m
       _ -> m
   processLiteral m _ = m

-- Perform boolean constraint propagation.
bcp :: DPLLState -> Maybe DPLLState
bcp (f, vs, i) = case findUnitClause $ cnfToList f of
                   -- There are no unit clauses
                   Nothing -> Just (f, vs, i)
                   -- There is some unit clause - Make the appropriate
                   -- assignment then try to perform BCP again.
                   Just (v, b) -> case assign v b (f, vs, i) of
                                    Nothing -> Nothing
                                    Just st -> bcp st
  where
    findUnitClause :: [[Literal]] -> Maybe (String, Bool)
    findUnitClause [] = Nothing
    findUnitClause ([LVar v] : _) = Just (v, True)
    findUnitClause ([LNot (LVar v)] : _) = Just (v, False)
    findUnitClause (_ : cs) = findUnitClause cs

-- The main DPLL function. Here cs is a set of clauses and vs is the set of
-- variables appearing in cs.
dpll :: DPLLState -> SATResult
dpll ds =
  case bcp ds >>= pureLiteralProp of
    Nothing -> Unsat
    Just (cs2, vs2, i2) ->
      if Set.null vs2
         then if cnfHasNoClauses cs2   -- empty set of clauses -> SAT
                 then Sat i2
                 else Unsat
         else let s = Set.findMin vs2
               in case assign s True (cs2, vs2, i2) of
                    Nothing -> Unsat
                    Just (cs3, vs3, i3) ->
                      case dpll (cs3, vs3, i3) of
                        Sat interp -> Sat interp
                        Unsat -> case assign s False (cs2, vs2, i2) of
                                   Nothing -> Unsat
                                   Just (cs4, vs4, i4) -> dpll (cs4, vs4, i4)

initialState :: CNFFormula -> DPLLState
initialState f = (f, collectVars f, emptyInterp)

-- Check the satisfiability of a set of clauses
checkCNF :: CNFFormula -> SATResult
checkCNF = dpll . initialState

collectFormVars :: Formula -> Set String
collectFormVars (And a b)     = Set.union (collectFormVars a) (collectFormVars b)
collectFormVars (Or a b)      = Set.union (collectFormVars a) (collectFormVars b)
collectFormVars (Implies a b) = Set.union (collectFormVars a) (collectFormVars b)
collectFormVars (Iff a b)     = Set.union (collectFormVars a) (collectFormVars b)
collectFormVars (Not a)       = collectFormVars a
collectFormVars (Lit l)       = case extractFromLit l of
                                  Nothing -> Set.empty
                                  Just s -> Set.singleton s
  where
    extractFromLit (LVar s) = Just s
    extractFromLit (LNot l) = extractFromLit l
    extractFromLit _ = Nothing

-- Check the satisfiability of a formula
checkSAT :: Formula -> SATResult
checkSAT f = let vs = collectFormVars f
              in case checkCNF $ tseitin f of
                   Unsat -> Unsat
                   Sat i -> Sat $ restrictInterpretation vs i
