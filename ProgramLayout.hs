module ProgramLayout where

import SymbolTable

-- Initialize registers and fix stack memory size
-- The stack here grows towards higher indices of the stack memory
vonNeumannInitCode = "#define N 20000\n" ++
                     "int sp, fp, r1, r2, r3;\n" ++
                     "int mem[N];\n\n" ++
                     "int main() {\n"

-- End code for the program
-- By default main returns zero as given in 'vonNeumannCode' function
vonNeumannExitCode = "_exit:;\n" ++
                     "\treturn mem[fp - 1];\n}\n"

-- Append Stack and Frame pointer initialization code based on the following information:
-- Reserve a subset of the initial memory to store global variables if they exist.
-- Reserve the next 2 locations to store the main function's return address and return value for compiler simplicity.
vonNeumannCode :: Table -> String -> String
vonNeumannCode (Table _ _ (Temps (Counts c1 _, _, _))) programCode = vonNeumannInitCode ++
                                   "\tfp = " ++ (show (2 + c1)) ++ ";\n" ++
                                   "\tsp = " ++ (show (2 + c1)) ++ ";\n" ++
                                   "\tmem[fp - 2] = &&_exit;\n" ++
                                   "\tmem[fp - 1] = 0;\n" ++
                                   "\tgoto _func_main;\n" ++
                                   programCode ++
                                   vonNeumannExitCode

-- Callee prologue code generator
prologue :: Table -> String -> String
prologue (Table _ _ (Temps (_, Counts c2 _, _))) idenValue = "_func_" ++ idenValue ++ ":;\n" ++
                                                             "\tfp = sp;\n" ++
                                                             "\tsp = fp + " ++ (show c2) ++ ";\n"

-- Callee epilogue code generator
epilogue :: Maybe String -> String
epilogue retMem = (retValCode retMem) ++ 
                  "\tsp = fp;\n" ++
                  "\tgoto *mem[fp - 2];\n"
  where retValCode Nothing = ""
        retValCode (Just id) = "\tmem[fp - 1] = " ++ id ++ ";\n"

-- Caller pre-jump code generator
preJump :: String -> [String] -> String
preJump label varList = (concat (zipWith assignParamsCode [0..] varList)) ++
                        (updateStackPointerCode varList) ++
                        "\tmem[sp] = fp;\n" ++
                        "\tmem[sp + 1] = &&" ++ label ++ ";\n" ++
                        "\tsp = sp + 3;\n"
  where updateStackPointerCode [] = ""
        updateStackPointerCode varList = "\tsp = sp + " ++ (show (length varList)) ++ ";\n"
        assignParamsCode x y = "\tmem[sp + " ++ (show x) ++ "] = " ++ y ++ ";\n"

-- Caller post-jump code generator
postJump :: Table -> String -> Maybe String -> String
postJump (Table _ _ (Temps (_, Counts c2 _, _))) label retMem = label ++ ":;\n" ++
                        "\tfp = mem[sp - 3];\n" ++
                        (retValCode retMem) ++
                        "\tsp = fp + " ++ (show c2) ++ ";\n"
  where retValCode Nothing = ""
        retValCode (Just id) = "\t" ++ id ++ " = mem[sp - 1];\n"

