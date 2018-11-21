module SymbolTable where

import Data.List

data Counts = Counts Int [(String, Int)]
data Temps = Temps (Counts, Counts, Counts)
data Table = Table Int Int Temps

-- Get a new Conditional label
getConditionalLabel :: Table -> (Table, String)
getConditionalLabel (Table lc1 lc2 t) = ((Table (lc1 + 1) lc2 t), "CONLABEL454" ++ (show lc1))

-- Get a new Function return label
getReturnLabel :: Table -> (Table, String)
getReturnLabel (Table lc1 lc2 t) = ((Table lc1 (lc2 + 1) t), "RETLABEL454" ++ (show lc2))

-- Add a variable/identifier to the current scope in the symbol table
addEntry :: Table -> String -> String -> Table
addEntry (Table lc1 lc2 (Temps (Counts c1 t1, Counts c2 t2, Counts c3 t3))) scope value
 | scope == "global" = (Table lc1 lc2 (Temps (Counts (c1 + 1) ((value, c1) : t1), Counts c2 t2, Counts c3 t3)))
 | scope == "local" = (Table lc1 lc2 (Temps (Counts c1 t1, Counts (c2 + 1) ((value, c2) : t2), Counts c3 t3)))
 | scope == "param" = (Table lc1 lc2 (Temps (Counts c1 t1, Counts c2 t2, Counts (c3 + 1) ((value, c3) : t3))))

-- Add a local temporary in the symbol table
addTemp :: Table -> (Table, String)
addTemp (Table lc1 lc2 (Temps (cs1, Counts c2 t2, cs3))) =
 (Table lc1 lc2 (Temps (cs1, Counts (c2 + 1) t2, cs3)), "mem[fp + " ++ (show c2) ++ "]")

-- Get corresponding memory for the variable/identifier in order of param -> local -> global
getMemVar :: Table -> String -> String
getMemVar (Table _ _ (Temps (Counts c1 t1, Counts c2 t2, Counts c3 t3))) value
 | any (checkup value) t3 =
   let (Just (_, index)) = find (checkup value) t3 in
   "mem[fp - 3 - " ++ (show (c3 - index)) ++ "]"
 | any (checkup value) t2 =
   let (Just (_, index)) = find (checkup value) t2 in
   "mem[fp + " ++ (show index) ++ "]"
 | any (checkup value) t1 =
   let (Just (_, index)) = find (checkup value) t1 in
   "mem[" ++ (show index) ++ "]"
 where checkup v (a, b) = if a == v then True else False

-- Get the count of variables/temporaries in the given scope
-- getDeclsCount :: Table -> String -> Int
-- getDeclsCount (Table _ _ (Temps (Counts c1 t1, Counts c2 t2, _))) scope
--  | scope == "global" = c1
-- | scope == "local" = c2
