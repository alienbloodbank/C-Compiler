module SymbolTable where

import Data.List
import Data.Map as Map

{-
SymbolTable.hs
Author: Soubhk Ghosh
11/19/2018
-}

data Counts = Counts Int (Map String Int) deriving Show
data Table = Table Int Int (Map String Int) (Map Int (Int, Counts, Counts))

-- Get a new Conditional label
getConditionalLabel :: Table -> (Table, String)
getConditionalLabel (Table lc1 lc2 fmap t) = ((Table (lc1 + 1) lc2 fmap t), "L" ++ (show lc1))

-- Get a new Function return label
getReturnLabel :: Table -> (Table, String)
getReturnLabel (Table lc1 lc2 fmap map1) = ((Table lc1 (lc2 + 1) fmap map1), "_ret" ++ (show lc2))

getScopeCount :: Table -> String -> Int
getScopeCount (Table lc1 lc2 fmap map1) idenVal = (let (Just nest) = (Map.lookup idenVal fmap) in nest)

addScope :: Table -> Int -> String -> (Table, Int)
addScope (Table lc1 lc2 fmap map1) nest idenValue =
  let nest1 = ((size fmap) + 1) in
  (Table lc1 lc2 (Map.insert idenValue nest1 fmap) (Map.insert nest1 (nest, Counts 0 Map.empty, Counts 0 Map.empty) map1), nest1)

-- Add a variable/identifier to the current scope in the symbol table
addEntry :: Table -> Int -> String -> String -> String -> Int -> Table
addEntry (Table lc1 lc2 fmap map1) nest scope value cline offset
 | scope == "local" =
   let (Just (outer, Counts c1 t1, cs2)) = (Map.lookup nest map1) in
   (Table lc1 lc2 fmap (Map.insert nest (outer, newTable c1 t1, cs2) map1))
 | scope == "param" =
   let (Just (outer, cs1, Counts c2 t2)) = (Map.lookup nest map1) in
   (Table lc1 lc2 fmap (Map.insert nest (outer, cs1, newTable c2 t2) map1))
 where newTable c t = Counts (c + offset) (Map.insert value c t)

-- Add a local temporary to the current scope in the symbol table
addTemp :: Table -> Int -> (Table, String)
addTemp (Table lc1 lc2 fmap map1) nest =
 let (Just (outer, Counts c1 t1, cs2)) = (Map.lookup nest map1) in
 (Table lc1 lc2 fmap (Map.insert nest (outer, Counts (c1 + 1) t1, cs2) map1), "mem[fp + " ++ (show c1) ++ "]")

traverseStaticLink :: Int -> String -> (Map Int (Int, Counts, Counts)) -> String -> (Int, Int)
traverseStaticLink nest value map1 cline
        | (Map.member value t2) =
           let (Just index) = (Map.lookup value t2) in
           (-4 - c2 + index, nest)
        | (Map.member value t1) =
           let (Just index) = (Map.lookup value t1) in
           (index, nest)
        | outer == -1 = errorWithoutStackTrace ("Semantic Error: line " ++ cline ++ ": '" ++ value ++ "' undeclared (first use in this function)")
        | otherwise = traverseStaticLink outer value map1 cline
          where (outer, Counts c1 t1, Counts c2 t2) = (let (Just subTable) = (Map.lookup nest map1) in subTable)

-- Get corresponding stack memory location for the variable/identifier by traversing the static chain
getMemVar :: Table -> Int -> String -> String -> (Table, String, String)
getMemVar (Table lc1 lc2 fmap map1) nest value cline =
  let (table1, label1) = (getConditionalLabel (Table lc1 lc2 fmap map1)) in
  let (table2, label2) = (getConditionalLabel table1) in
  let (table3, label3) = (getConditionalLabel table2) in
  let (index, currentNest) = (traverseStaticLink nest value map1 cline) in
  let regCode = "\tr3 = fp;\n" ++
                label3 ++ ":\n\t;\n" ++
                "\tif(mem[r3 - 1] != " ++ (show currentNest) ++ ") goto " ++ label1 ++ ";\n" ++
                "\tgoto " ++ label2 ++ ";\n" ++
                label1 ++ ":\n\t;\n" ++
                "\tr3 = mem[r3 - 4];\n" ++
                "\tgoto " ++ label3 ++ ";\n" ++
                label2 ++ ":\n\t;\n" ++
                "\tr3 = r3 + " ++ (show index) ++ ";\n" in
  (table3, "mem[r3]", regCode)

getMemArrayVar :: Table -> Int -> String -> String -> String -> (Table, String, String)
getMemArrayVar (Table lc1 lc2 fmap map1) nest value code cline =
  let (table1, label1) = (getConditionalLabel (Table lc1 lc2 fmap map1)) in
  let (table2, label2) = (getConditionalLabel table1) in
  let (table3, label3) = (getConditionalLabel table2) in
  let (index, currentNest) = (traverseStaticLink nest value map1 cline) in
  let regCode = "\tr3 = fp;\n" ++
                label3 ++ ":\n\t;\n" ++
                "\tif(mem[r3 - 1] != " ++ (show currentNest) ++ ") goto " ++ label1 ++ ";\n" ++
                "\tgoto " ++ label2 ++ ";\n" ++
                label1 ++ ":\n\t;\n" ++
                "\tr3 = mem[r3 - 4];\n" ++
                "\tgoto " ++ label3 ++ ";\n" ++
                label2 ++ ":\n\t;\n" ++
                "\tr3 = r3 + " ++ (show index) ++ ";\n" ++
                "\tr3 = r3 + " ++ code ++ ";\n" in
  (table3, "mem[r3]", regCode)
