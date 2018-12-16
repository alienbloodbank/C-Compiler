module ProgramLayout where

import SymbolTable

import Data.List (isSuffixOf)
import Data.Map as Map

{-
ProgramLayout.hs
Author: Soubhk Ghosh
11/19/2018
-}

-- Initialize registers and fix stack memory size
-- The stack here grows towards higher indices of the stack memory
vonNeumannInitCode = "#define STACK_SIZE 20000\n" ++
                     "int sp, fp, r1, r2, r3;\n" ++
                     "int mem[STACK_SIZE];\n\n" ++
                     "int main() {\n"

-- End code for the program
-- By default main returns zero as given in 'vonNeumannCode' function
vonNeumannExitCode = "_overflow:\n\t;\n" ++
                     "\tprintf(\"stack overflow\\n\");\n\treturn 1;\n" ++
                     "_exit:\n\t;\n" ++
                     "\treturn mem[fp - 2];\n}\n"

checkStackOverflow :: String
checkStackOverflow = "\tif(sp >= STACK_SIZE) goto _overflow;\n"

-- Update stack pointer whenever a temporary is created
growStackFromTemp :: Table -> Int -> String
growStackFromTemp (Table _ _ fmap map1) nest = "\tsp = fp + " ++ (show c1) ++ ";\n" ++ checkStackOverflow
   where (_, Counts c1 t1, _) = (let (Just subTable) = (Map.lookup nest map1) in subTable)

-- Append Stack and Frame pointer initialization code based on the following information:
-- Reserve a subset of the initial memory to store global variables if they exist.
-- Reserve the next 2 locations to store the main function's return address and return value for compiler simplicity.
vonNeumannCode :: Table -> [String] -> String
vonNeumannCode (Table _ _ fmap map1) funcList = vonNeumannInitCode ++
                                   "\tfp = " ++ (show (c1 + 4)) ++ ";\n" ++
                                   "\tsp = fp;\n" ++
                                   "\tmem[fp - 4] = 0;\n" ++
                                   "\tmem[fp - 3] = &&_exit;\n" ++
                                   "\tmem[fp - 2] = 0;\n" ++
                                   "\tmem[fp - 1] = " ++ (show mainNest) ++ ";\n" ++
                                   "\tgoto _func_main;\n" ++
                                   (concat funcList) ++
                                   vonNeumannExitCode
   where (_, Counts c1 t1, _) = (let (Just subTable) = (Map.lookup 0 map1) in subTable)
         (Just mainNest) = (Map.lookup "main" fmap)

-- Callee prologue code generator
prologue :: Table -> Int -> String -> String
prologue (Table _ _ _ map1) nest idenValue = "_func_" ++ idenValue ++ ":\n\t;\n" ++
                                             "\tfp = sp;\n" ++
                                             "\tsp = fp + " ++ (show c1) ++ ";\n" ++ checkStackOverflow
   where (_, Counts c1 t1, _) = (let (Just subTable) = (Map.lookup nest map1) in subTable)

-- Callee epilogue code generator
epilogue :: Maybe String -> String
epilogue retMem = (retValCode retMem) ++ 
                  "\tsp = fp;\n" ++
                  "\tgoto *mem[fp - 3];\n"
  where retValCode Nothing = ""
        retValCode (Just id) = "\tmem[fp - 2] = " ++ id ++ ";\n"

-- Caller pre-jump code generator
preJump :: Int -> String -> [String] -> [String] -> String
preJump nest label varList indList = "\tsp = sp + " ++ (show (length varList)) ++ ";\n" ++ checkStackOverflow ++
                        (concat (zipWith3 assignParamsCode [1..] (reverse varList) (reverse indList))) ++
                        "\tsp = sp + 4;\n" ++ checkStackOverflow ++
                        "\tmem[sp - 4] = fp;\n" ++
                        "\tmem[sp - 3] = &&" ++ label ++ ";\n" ++
                        "\tmem[sp - 1] = " ++ (show nest) ++ ";\n"
  where assignParamsCode x y z = z ++ "\tmem[sp - " ++ (show x) ++ "] = " ++ y ++ ";\n"

-- Caller post-jump code generator
postJump :: Table -> Int -> String -> Maybe String -> String
postJump (Table _ _ _ map1) nest label retMem = label ++ ":\n\t;\n" ++
                        "\tfp = mem[sp - 4];\n" ++
                        (retValCode retMem) ++
                        "\tsp = fp + " ++ (show c1) ++ ";\n" ++ checkStackOverflow
  where retValCode Nothing = ""
        retValCode (Just id) = "\t" ++ id ++ " = mem[sp - 2];\n"
        (_, Counts c1 t1, _) = (let (Just subTable) = (Map.lookup nest map1) in subTable)

-- Adds the callee epilogue code incase the programmer hasn't explicitly added the return statement
funcTailCode :: String -> String
funcTailCode code
  | "*mem[fp - 3];\n" `isSuffixOf` code = code
  | otherwise = code ++ (epilogue Nothing)

