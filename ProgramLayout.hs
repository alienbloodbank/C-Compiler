module ProgramLayout where

import SymbolTable

{-
ProgramLayout.hs
Author: Soubhk Ghosh
11/19/2018
-}

-- Initialize registers and fix stack memory size
-- The stack here grows towards higher indices of the stack memory
vonNeumannInitCode = "#define N 20000\n" ++
                     "int sp, fp, r1, r2, r3;\n" ++
                     "int mem[N];\n\n" ++
                     "int main() {\n"

-- End code for the program
-- By default main returns zero as given in 'vonNeumannCode' function
vonNeumannExitCode = "_overflow:\n\t;\n" ++
                     "\tprintf(\"Stack Overflow\\n\");\n\treturn 1;\n" ++
                     "_exit:\n\t;\n" ++
                     "\treturn mem[fp - 1];\n}\n"

checkStackOverflow :: String
checkStackOverflow = "\tif(sp >= 20000) goto _overflow;\n"

-- Update stack pointer whenever a temporary is created
growStackFromTemp :: Table -> String
growStackFromTemp (Table _ _ (Temps (_, Counts c2 _, _))) = "\tsp = fp + " ++ (show c2) ++ ";\n" ++ checkStackOverflow

-- Append Stack and Frame pointer initialization code based on the following information:
-- Reserve a subset of the initial memory to store global variables if they exist.
-- Reserve the next 2 locations to store the main function's return address and return value for compiler simplicity.
vonNeumannCode :: Table -> String -> String
vonNeumannCode (Table _ _ (Temps (Counts c1 _, _, _))) programCode = vonNeumannInitCode ++
                                   "\tfp = " ++ (show (c1 + 2)) ++ ";\n" ++
                                   "\tsp = " ++ (show (c1 + 2)) ++ ";\n" ++
                                   "\tmem[fp - 2] = &&_exit;\n" ++
                                   "\tmem[fp - 1] = 0;\n" ++
                                   "\tgoto _func_main;\n" ++
                                   programCode ++
                                   vonNeumannExitCode

-- Callee prologue code generator
prologue :: Table -> String -> String
prologue (Table _ _ (Temps (_, Counts c2 _, _))) idenValue = "_func_" ++ idenValue ++ ":\n\t;\n" ++
                                                             "\tfp = sp;\n" ++
                                                             "\tsp = fp + " ++ (show c2) ++ ";\n" ++ checkStackOverflow

-- Callee epilogue code generator
epilogue :: Maybe String -> String
epilogue retMem = (retValCode retMem) ++ 
                  "\tsp = fp;\n" ++
                  "\tgoto *mem[fp - 2];\n"
  where retValCode Nothing = ""
        retValCode (Just id) = "\tmem[fp - 1] = " ++ id ++ ";\n"

-- Caller pre-jump code generator
preJump :: String -> [String] -> String
preJump label varList = "\tsp = sp + " ++ (show (length varList)) ++ ";\n" ++ checkStackOverflow ++
                        (concat (zipWith assignParamsCode [1..] (reverse varList))) ++
                        "\tsp = sp + 3;\n" ++ checkStackOverflow ++
                        "\tmem[sp - 3] = fp;\n" ++
                        "\tmem[sp - 2] = &&" ++ label ++ ";\n"
  where assignParamsCode x y = "\tmem[sp + " ++ (show (-x)) ++ "] = " ++ y ++ ";\n"

-- Caller post-jump code generator
postJump :: Table -> String -> Maybe String -> String
postJump (Table _ _ (Temps (_, Counts c2 _, _))) label retMem = label ++ ":\n\t;\n" ++
                        "\tfp = mem[sp - 3];\n" ++
                        (retValCode retMem) ++
                        "\tsp = fp + " ++ (show c2) ++ ";\n"
  where retValCode Nothing = ""
        retValCode (Just id) = "\t" ++ id ++ " = mem[sp - 1];\n"

