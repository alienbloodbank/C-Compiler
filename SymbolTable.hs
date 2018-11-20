module SymbolTable where

import Data.List

data Counts = Counts Int [(String, Int)]
data Temps = Temps (Counts, Counts, [String])
data Table = Table Int Temps

-- Get a new label
getLabel :: Table -> (Table, String)
getLabel (Table a b) = ((Table (a + 1) b), "LABEL_" ++ (show a))

-- Add a variable/identifier to the current scope in the symbol table
addEntry :: Table -> String -> String -> Table
addEntry (Table lc (Temps (Counts c1 t1, Counts c2 t2, t3))) scope value
 | scope == "global" = (Table lc (Temps (Counts (c1 + 1) ((value, c1):t1), Counts c2 t2, t3)))
 | scope == "local" = (Table lc (Temps (Counts c1 t1, Counts (c2 + 1) ((value, c2):t2), t3)))
 | scope == "param" = (Table lc (Temps (Counts c1 t1, Counts c2 t2, value:t3)))

-- Add a local temporary in the symbol table 
addTemp :: Table -> (Table, String)
addTemp (Table lc (Temps (Counts c1 t1, Counts c2 t2, t3))) =
 (Table lc (Temps (Counts c1 t1, Counts (c2+1) t2, t3)), "local[" ++ (show c2) ++ "]")

-- Get corresponding temporary for the variable/identifier in order of param -> local -> global
getVar :: Table -> String -> String
getVar (Table lc (Temps (Counts c1 t1, Counts c2 t2, t3))) value
 | value `elem` t3 = value
 | any (checkup value) t2 = 
   let (Just (_, index)) = find (checkup value) t2 in
   "local[" ++ (show index) ++ "]"
 | any (checkup value) t1 = 
   let (Just (_, index)) = find (checkup value) t1 in
   "global[" ++ (show index) ++ "]"
 where checkup v (a, b) = if a == v then True else False

-- Print variable/identifier declarations
programDecls :: Table -> String -> String
programDecls (Table lc (Temps (Counts c1 t1, Counts c2 t2, t3))) scope
 | scope == "global" = decls "global" c1
 | scope == "local" = decls "local" c2
 where decls s c = if c > 0 then ("int " ++ s ++ "[" ++ (show c) ++ "];\n") else ""
