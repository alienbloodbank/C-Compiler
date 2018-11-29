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
vonNeumannExitCode = "mainReturn:;\n" ++
                     "return mem[fp - 1];\n}\n"

-- Append Stack and Frame pointer initialization code based on the following information:
-- Reserve a subset of the initial memory to store global variables if they exist.
-- Reserve the next 2 locations to store the main function's return address and return value for compiler simplicity.
vonNeumannCode :: Table -> String -> String
vonNeumannCode (Table _ _ (Temps (Counts c1 _, _, _))) programCode = vonNeumannInitCode ++
                                   "fp = 2 + " ++ (show c1) ++ ";\n" ++
                                   "sp = 2 + " ++ (show c1) ++ ";\n" ++
                                   "mem[fp - 2] = &&mainReturn;\n" ++
                                   "mem[fp - 1] = 0;\n" ++
                                   "goto mainFunc;\n" ++
                                   programCode ++
                                   vonNeumannExitCode

-- Callee prologue code generator
prologue :: Table -> String -> String
prologue (Table _ _ (Temps (_, Counts c2 _, _))) idenValue = idenValue ++ "Func:;\n" ++
                                                             "fp = sp;\n" ++
                                                             "sp = fp + " ++ (show c2) ++ ";\n"

-- Callee epilogue code generator
epilogue :: Maybe String -> String
epilogue retMem = (retValCode retMem) ++ 
                  "sp = fp;\n" ++ 
                  "goto *mem[fp - 2];\n" 
  where retValCode Nothing = "\n"
        retValCode (Just id) = "mem[fp - 1] = " ++ id ++ ";\n"

-- Caller pre-jump code generator
preJump :: String -> [String] -> String
preJump label varList = (concat (zipWith assignParamsCode [0..] varList)) ++
                        (updateStackPointerCode varList) ++
                        "mem[sp] = fp;\n" ++
                        "mem[sp + 1] = &&" ++ label ++ ";\n" ++
                        "sp = sp + 3;\n"
  where updateStackPointerCode [] = "\n"
        updateStackPointerCode varList = "sp = sp + " ++ (show (length varList)) ++ ";\n" 
        assignParamsCode x y = "mem[sp + " ++ (show x) ++ "] = " ++ y ++ ";\n" 

-- Caller post-jump code generator
-- When we generate post-jump code, we don't know how many local variables will be there yet after callee returns.
-- Hence we add a temporary string "$LOCAL_COUNT$" that is replaced after the complete function defintion is parsed and analysed.
postJump :: Table -> String -> Maybe String -> String
postJump (Table _ _ (Temps (_, Counts c2 _, _))) label retMem = label ++ ":;\n" ++
                        "fp = mem[sp - 3];\n" ++
                        (retValCode retMem) ++
                        "sp = fp + " ++ (show c2) ++ ";\n"
  where retValCode Nothing = "\n"
        retValCode (Just id) = id ++ " = mem[sp - 1];\n"

