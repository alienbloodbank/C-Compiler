module SymbolTable where

import Data.List
import Data.Map as Map

{-
SymbolTable.hs
Author: Soubhk Ghosh
11/19/2018
-}

data Counts = Counts Int (Map String Int) deriving Show
data Temps = Temps (Counts, Counts, Counts)
data Table = Table Int Int Temps

-- Get a new Conditional label
getConditionalLabel :: Table -> (Table, String)
getConditionalLabel (Table lc1 lc2 t) = ((Table (lc1 + 1) lc2 t), "L" ++ (show lc1))

-- Get a new Function return label
getReturnLabel :: Table -> (Table, String)
getReturnLabel (Table lc1 lc2 t) = ((Table lc1 (lc2 + 1) t), "_ret" ++ (show lc2))

-- Add a variable/identifier to the current scope in the symbol table
addEntry :: Table -> String -> String -> String -> Int ->  Table
addEntry (Table lc1 lc2 (Temps (Counts c1 t1, Counts c2 t2, Counts c3 t3))) scope value cline offset
 | (&&) (scope == "global") (not (Map.member value t1)) = (Table lc1 lc2 (Temps (newTable c1 t1, Counts c2 t2, Counts c3 t3)))
 | not ((Map.member value t2) || (Map.member value t3)) =
    case scope of "local" -> (Table lc1 lc2 (Temps (Counts c1 t1, newTable c2 t2, Counts c3 t3)))
                  "param" -> (Table lc1 lc2 (Temps (Counts c1 t1, Counts c2 t2, newTable c3 t3)))
 | otherwise = errorWithoutStackTrace ("Semantic Error: line " ++ cline ++ ": '" ++ value ++ "' redeclared as different kind of symbol")
 where newTable c t = Counts (c + offset) (Map.insert value c t)

-- Add a local temporary in the symbol table
addTemp :: Table -> (Table, String)
addTemp (Table lc1 lc2 (Temps (cs1, Counts c2 t2, cs3))) =
 (Table lc1 lc2 (Temps (cs1, Counts (c2 + 1) t2, cs3)), "mem[fp + " ++ (show c2) ++ "]")

-- Get corresponding stack memory location for the variable/identifier in the priority of param -> local -> global
getMemVar :: Table -> String -> String -> String
getMemVar (Table _ _ (Temps (Counts c1 t1, Counts c2 t2, Counts c3 t3))) value cline
 | (Map.member value t3) =
   let (Just index) = (Map.lookup value t3) in
   "mem[fp - " ++ (show (3 + c3 - index)) ++ "]"
 | (Map.member value t2) =
   let (Just index) = (Map.lookup value t2) in
   "mem[fp + " ++ (show index) ++ "]"
 | (Map.member value t1) =
   let (Just index) = (Map.lookup value t1) in
   "mem[" ++ (show index) ++ "]"
 | otherwise = errorWithoutStackTrace ("Semantic Error: line " ++ cline ++ ": '" ++ value ++ "' undeclared (first use in this function)")

getMemArrayVar :: Table -> String -> String -> String -> String -> (String, String)
getMemArrayVar table value fn code cline
 | (Map.member value t2) =
   let (Just index) = (Map.lookup value t2) in
   let fn1 = (fn ++ "\tr3 = " ++ (show index) ++ " + " ++ code ++ ";\n") in
   ("mem[fp + r3]", fn1)
 | (Map.member value t1) =
   let (Just index) = (Map.lookup value t1) in
   let fn1 = (fn ++ "\tr3 = " ++ (show index) ++ " + " ++ code ++ ";\n") in
   ("mem[r3]", fn1)
 | otherwise = errorWithoutStackTrace ("Semantic Error: line " ++ cline ++ ": '" ++ value ++ "' undeclared (first use in this function)")
 where (Table lc1 lc2 (Temps (Counts c1 t1, Counts c2 t2, cs3))) = table
