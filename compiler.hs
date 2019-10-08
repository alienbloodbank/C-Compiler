import Scanner
import ParseTable
import SymbolTable
import ProgramLayout
import Evaluator

import System.Environment (getArgs)
import Data.Map as Map

{-
compiler.hs
Author: Soubhk Ghosh
11/19/2018
-}

-- Token consumer
match :: TokenIterator -> String -> TokenIterator
match (currentToken, rest, cline) expected
  | (tokenType currentToken) == expected = nextToken rest cline
  | otherwise = (reportError [currentToken] (show cline) expected)

-- program -> program_data EOF
program :: TokenIterator -> Table -> (TokenIterator, String, Table)
program (currentToken, rest, cline) table
  | (tokenType currentToken) `elem` (predict "program_data") = 
    let (ti1, table1, funcList1) = (program_data (currentToken, rest, cline) 0 table []) in
    let ti2 = (match ti1 "EOF") in
    (ti2, (vonNeumannCode table1 funcList1), table1)
  | otherwise = (reportError (predict "program") (show cline) currentToken)

-- program_data -> ε | global_decl program_data
program_data :: TokenIterator -> Int -> Table -> [String] -> (TokenIterator, Table, [String])
program_data (currentToken, rest, cline) nest table funcList
  | (tokenType currentToken) `elem` (parseTable "program_data" "FOLLOW") =
    ((currentToken, rest, cline), table, funcList)
  | (tokenType currentToken) `elem` (predict "global_decl") =
    let (ti1, table1, funcList1) = (global_decl (currentToken, rest, cline) nest table funcList) in
    let (ti2, table2, funcList2) = (program_data ti1 nest table1 funcList1) in
    (ti2, table2, funcList2)
  | otherwise = (reportError (predict "program_data") (show cline) currentToken)

-- global_decl -> type_name identifier global_decl_tail
global_decl :: TokenIterator -> Int -> Table -> [String] -> (TokenIterator, Table, [String])
global_decl (currentToken, rest, cline) nest table funcList
  | (tokenType currentToken) `elem` (predict "type_name") =
    let ti1 = (type_name (currentToken, rest, cline)) in
    let ti2 = (match ti1 "identifier") in
    let (ti3, table1, funcList1) = (global_decl_tail ti2 nest table (extractValue (tiHead ti1)) funcList) in
    (ti3, table1, funcList1)
  | otherwise = (reportError (predict "global_decl") (show cline) currentToken)

-- global_decl_tail -> id_list_tail ; | [ expression ] id_list_tail ; | ( parameter_list ) func_tail
global_decl_tail :: TokenIterator -> Int -> Table -> String -> [String] -> (TokenIterator, Table, [String])
global_decl_tail (currentToken, rest, cline) nest table idenValue funcList
  | (tokenType currentToken) `elem` (predict "id_list_tail") =
    let table1 = (addEntry table nest "local" idenValue (show cline) 1) in
    let (ti1, table2) = (id_list_tail (currentToken, rest, cline) nest table1 "local") in
    let ti2 = (match ti1 ";") in
    (ti2, table2, funcList)
  | (tokenType currentToken) == "[" =
    let ti1 = (match (currentToken, rest, cline) "[") in
    let (ti2, arraySize) = (evaluateConstant ti1) in
    let ti3 = (match ti2 "]") in
    let table1 = (addEntry table nest "local" idenValue (show cline) arraySize) in
    let (ti4, table2) = (id_list_tail ti3 nest table1 "local") in
    let ti5 = (match ti4 ";") in
    (ti5, table2, funcList)
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (table1, nest1) = (addScope table nest idenValue) in
    let (ti2, table2) = (parameter_list ti1 nest1 table1) in
    let ti3 = (match ti2 ")") in
    let (ti4, table3, funcList1) = (func_tail ti3 nest1 table2 idenValue funcList) in
    (ti4, table3, funcList1)
  | otherwise = (reportError (predict "global_decl_tail") (show cline) currentToken)

-- func_tail -> ; | { program_data statements }
func_tail :: TokenIterator -> Int -> Table -> String -> [String] -> (TokenIterator, Table, [String])
func_tail (currentToken, rest, cline) nest table idenValue funcList
  | (tokenType currentToken) == ";" =
    let ti1 = (match (currentToken, rest, cline) ";") in
    (ti1, table, funcList)
  | (tokenType currentToken) == "{" =
    let ti1 = (match (currentToken, rest, cline) "{") in
    let (ti2, table1, funcList1) = (program_data ti1 nest table funcList) in
    let (ti3, code1, table2) = (statements ti2 nest table1 []) in
    let ti4 = (match ti3 "}") in
    (ti4, table2, ((prologue table1 nest idenValue) ++ (funcTailCode code1)) : funcList1)
  | otherwise = (reportError (predict "func_tail") (show cline) currentToken)
  
-- type_name -> int | void
type_name :: TokenIterator -> TokenIterator
type_name (currentToken, rest, cline)
  | (tokenType currentToken) == "int" =
    (match (currentToken, rest, cline) "int")
  | (tokenType currentToken) == "void" =
    (match (currentToken, rest, cline) "void")
  | otherwise = (reportError (predict "type_name") (show cline) currentToken)

-- parameter_list -> ε | void parameter_list_tail | int identifierNT non_empty_parameter_list
parameter_list :: TokenIterator -> Int -> Table -> (TokenIterator, Table)
parameter_list (currentToken, rest, cline) nest table
  | (tokenType currentToken) `elem` (parseTable "parameter_list" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | (tokenType currentToken) == "void" =
    let ti1 = (match (currentToken, rest, cline) "void") in
    let (ti2, table1) = (parameter_list_tail ti1 nest table "void") in
    (ti2, table1)
  | (tokenType currentToken) == "int" =
    let ti1 = (match (currentToken, rest, cline) "int") in
    let (ti2, _) = (identifierNT ti1) in
    let table1 = (addEntry table nest "param" (extractValue (tiHead ti1)) (show cline) 1) in
    let (ti3, table2) = (non_empty_parameter_list ti2 nest table1) in
    (ti3, table2)
  | otherwise = (reportError (predict "parameter_list") (show cline) currentToken)
  
-- parameter_list_tail -> ε | identifierNT non_empty_parameter_list
parameter_list_tail :: TokenIterator -> Int -> Table -> String -> (TokenIterator, Table)
parameter_list_tail (currentToken, rest, cline) nest table typeNameCode
  | (tokenType currentToken) `elem` (parseTable "parameter_list_tail" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | (tokenType currentToken) `elem` (predict "identifierNT") =
    let (ti1, _) = (identifierNT (currentToken, rest, cline)) in
    let table1 = (addEntry table nest "param" (extractValue currentToken) (show cline) 1) in
    let (ti2, table2) = (non_empty_parameter_list ti1 nest table1) in
    (ti2, table2)
  | otherwise = (reportError (predict "parameter_list_tail") (show cline) currentToken)

-- non_empty_parameter_list -> ε | , type_name identifierNT non_empty_parameter_list
non_empty_parameter_list :: TokenIterator -> Int -> Table -> (TokenIterator, Table)
non_empty_parameter_list (currentToken, rest, cline) nest table
  | (tokenType currentToken) `elem` (parseTable "non_empty_parameter_list" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let ti2 = (type_name ti1) in
    let (ti3, _) = (identifierNT ti2) in
    let table1 = (addEntry table nest "param" (extractValue (tiHead ti2)) (show cline) 1) in
    let (ti4, table2) = (non_empty_parameter_list ti3 nest table1) in
    (ti4, table2)
  | otherwise = (reportError (predict "non_empty_parameter_list") (show cline) currentToken)

-- id_list_tail -> ε | , identifierNT id_list_tail
id_list_tail :: TokenIterator -> Int -> Table -> String -> (TokenIterator, Table)
id_list_tail (currentToken, rest, cline) nest table scope
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let (ti2, arraySize) = (identifierNT ti1) in
    let table1 = (addEntry table nest scope (extractValue (tiHead ti1)) (show cline) arraySize) in
    let (ti3, table2) = (id_list_tail ti2 nest table1 scope) in
    (ti3, table2)
  | (tokenType currentToken) `elem` (parseTable "id_list_tail" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | otherwise = (reportError (predict "id_list_tail") (show cline) currentToken)

-- identifierNT	-> identifier identifierNT_tail
identifierNT :: TokenIterator -> (TokenIterator, Int)
identifierNT (currentToken, rest, cline)
  | (tokenType currentToken) == "identifier" =
    let ti1 = (match (currentToken, rest, cline) "identifier") in
    let (ti2, arraySize) = (identifierNT_tail ti1) in
    (ti2, arraySize)
  | otherwise = (reportError (predict "identifierNT") (show cline) currentToken)

-- identifierNT_tail -> ε | [ expression ]
identifierNT_tail :: TokenIterator -> (TokenIterator, Int)
identifierNT_tail (currentToken, rest, cline)
  | (tokenType currentToken) == "[" =
    let ti1 = (match (currentToken, rest, cline) "[") in
    let (ti2, arraySize) = (evaluateConstant ti1) in
    let ti3 = (match ti2 "]") in
    (ti3, arraySize)
  | (tokenType currentToken) `elem` (parseTable "identifierNT_tail" "FOLLOW") =
    ((currentToken, rest, cline), 1)
  | otherwise = (reportError (predict "identifierNT_tail") (show cline) currentToken)

-- block_statements -> { statements }
block_statements :: TokenIterator -> Int -> Table -> [String] -> (TokenIterator, String, Table)
block_statements (currentToken, rest, cline) nest table bc_label
  | (tokenType currentToken) == "{" =
    let ti1 = (match (currentToken, rest, cline) "{") in
    let (ti2, code1, table1) = (statements ti1 nest table bc_label) in
    let ti3 = (match ti2 "}") in
    (ti3, code1, table1)
  | otherwise = (reportError (predict "block_statements") (show cline) currentToken)
  
-- statements -> ε | statement statements
statements :: TokenIterator -> Int -> Table -> [String] -> (TokenIterator, String, Table)
statements (currentToken, rest, cline) nest table bc_label
  | (tokenType currentToken) `elem` (parseTable "statements" "FOLLOW") =
    ((currentToken, rest, cline), "", table)
  | (tokenType currentToken) `elem` (predict "statement") =
    let (ti1, code1, table1, fn1) = (statement (currentToken, rest, cline) nest table "" bc_label) in
    let (ti2, code2, table2) = (statements ti1 nest table1 bc_label) in
    (ti2, fn1 ++ code1 ++ code2, table2)
  | otherwise = (reportError (predict "statements") (show cline) currentToken)
  
{-
statement ->            assignment_or_general_func_call |
			printf_func_call | 
			scanf_func_call | 
                        read_func_call |
                        write_func_call |
			if_statement | 
			while_statement	| 
			return_statement | 
			break_statement | 
			continue_statement  
-}
statement :: TokenIterator -> Int -> Table -> String -> [String] -> (TokenIterator, String, Table, String)
statement (currentToken, rest, cline) nest table fn bc_label
  | (tokenType currentToken) `elem` (predict "assignment_or_general_func_call") =
    let (ti1, code1, table1, fn1) = (assignment_or_general_func_call (currentToken, rest, cline) nest table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "printf_func_call") =
    let (ti1, code1, table1, fn1) = (printf_func_call (currentToken, rest, cline) nest table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "scanf_func_call") =
    let (ti1, code1, table1, fn1) = (scanf_func_call (currentToken, rest, cline) nest table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "read_func_call") =
    let (ti1, code1, table1, fn1) = (read_func_call (currentToken, rest, cline) nest table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "write_func_call") =
    let (ti1, code1, table1, fn1) = (write_func_call (currentToken, rest, cline) nest table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "if_statement") =
    let (ti1, code1, table1, fn1) = (if_statement (currentToken, rest, cline) nest table fn bc_label) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "while_statement") =
    let (ti1, code1, table1, fn1) = (while_statement (currentToken, rest, cline) nest table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "return_statement") =
    let (ti1, code1, table1, fn1) = (return_statement (currentToken, rest, cline) nest table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "break_statement") =
    let (ti1, code1, table1) = (break_statement (currentToken, rest, cline) table bc_label) in
    (ti1, code1, table1, fn)
  | (tokenType currentToken) `elem` (predict "continue_statement") =
    let (ti1, code1, table1) = (continue_statement (currentToken, rest, cline) table bc_label) in
    (ti1, code1, table1, fn)
  | otherwise = (reportError (predict "statement") (show cline) currentToken)
  
-- assignment_or_general_func_call -> identifier assignment_or_general_func_call_tail
assignment_or_general_func_call :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
assignment_or_general_func_call (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "identifier" =
    let ti1 = (match (currentToken, rest, cline) "identifier") in
    let (ti2, code1, table1, fn1) = (assignment_or_general_func_call_tail ti1 nest table fn (extractValue currentToken)) in
    (ti2, code1, table1, fn1)
  | otherwise = (reportError (predict "assignment_or_general_func_call") (show cline) currentToken)

-- assignment_or_general_func_call_tail -> [expression] = expression ; | = expression ; | ( expr_list ) ;
assignment_or_general_func_call_tail :: TokenIterator -> Int -> Table -> String -> String -> (TokenIterator, String, Table, String)
assignment_or_general_func_call_tail (currentToken, rest, cline) nest table fn idenValue
  | (tokenType currentToken) == "[" =
    let ti1 = (match (currentToken, rest, cline) "[") in
    let (ti2, code1, table1, fn1, indfn1) = (expression ti1 nest table fn) in
    let ti3 = (match ti2 "]") in
    let ti4 = (match ti3 "=") in
    let (ti5, code2, table2, fn2, indfn2) = (expression ti4 nest table1 fn1) in
    let ti6 = (match ti5 ";") in
    let (table3, named_var1) = (addTemp table2 nest) in
    let (table4, named_var2, indfn3) = (getMemArrayVar table3 nest idenValue named_var1 (show cline)) in
    let regCode = indfn2 ++ "\tr1 = " ++ code2 ++ ";\n" ++ indfn1 ++ "\t" ++ named_var1 ++ " = " ++ code1 ++ ";\n" ++ indfn3 ++
                  "\t" ++ named_var2 ++ " = r1;\n" in
    (ti6, regCode, table4, fn2 ++ (growStackFromTemp table4 nest))
  | (tokenType currentToken) == "=" =
    let ti1 = (match (currentToken, rest, cline) "=") in
    let (ti2, code1, table1, fn1, indfn1) = (expression ti1 nest table fn) in
    let ti3 = (match ti2 ";") in
    let (table2, named_var, indfn3) = (getMemVar table1 nest idenValue (show cline)) in
    let regCode = indfn1 ++ "\tr1 = " ++ code1 ++ ";\n" ++ indfn3 ++
                  "\t" ++ named_var ++ " = r1;\n" in
    (ti3, regCode, table2, fn1)
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (ti2, varList1, indfnList1, table1, fn1) = (expr_list ti1 nest table fn) in
    let ti3 = (match ti2 ")") in
    let ti4 = (match ti3 ";") in
    let (table2, retLabel) = (getReturnLabel table1) in  
    let nest1 = (getScopeCount table2 idenValue) in
    (ti4, (preJump nest1 retLabel varList1 indfnList1) ++ "\tgoto _func_" ++ idenValue ++ ";\n"  ++ (postJump table2 nest retLabel Nothing), table2, fn1)
  | otherwise = (reportError (predict "assignment_or_general_func_call_tail") (show cline) currentToken)

-- printf_func_call -> printf ( STRING printf_func_call_tail   
printf_func_call :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
printf_func_call (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "printf" =
    let ti1 = (match (currentToken, rest, cline) "printf") in
    let ti2 = (match ti1 "(") in
    let ti3 = (match ti2 "string") in
    let (ti4, code1, table1, fn1) = (printf_func_call_tail ti3 nest table fn) in
    (ti4, "\tprintf(" ++ (extractValue (tiHead ti2)) ++ code1, table1, fn1)
  | otherwise = (reportError (predict "printf_func_call") (show cline) currentToken)

-- printf_func_call_tail -> ) ; | , expression ) ;
printf_func_call_tail :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
printf_func_call_tail (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == ")" =
    let ti1 = (match (currentToken, rest, cline) ")") in
    let ti2 = (match ti1 ";") in
    (ti2, ");\n", table, fn)
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let (ti2, code1, table1, fn1, indfn1) = (expression ti1 nest table fn) in
    let ti3 = (match ti2 ")") in
    let ti4 = (match ti3 ";") in
    (ti4, ", " ++ code1 ++ ");\n", table1, fn1 ++ indfn1)
  | otherwise = (reportError (predict "printf_func_call_tail") (show cline) currentToken)

-- scanf_func_call -> scanf ( string , &expression ) ;
scanf_func_call :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
scanf_func_call (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "scanf" =
    let ti1 = (match (currentToken, rest, cline) "scanf") in
    let ti2 = (match ti1 "(") in
    let ti3 = (match ti2 "string") in
    let ti4 = (match ti3 ",") in
    let ti5 = (match ti4 "&") in
    let (ti6, code1, table1, fn1, indfn1) = (expression ti5 nest table fn) in
    let ti7 = (match ti6 ")") in
    let ti8 = (match ti7 ";") in
    (ti8, "\tscanf(" ++ (extractValue (tiHead ti2)) ++ ", &" ++ code1 ++ ");\n", table1, fn1 ++ indfn1)
  | otherwise = (reportError (predict "scanf_func_call") (show cline) currentToken)

-- read_func_call -> read ( expression ) ;
read_func_call :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
read_func_call (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "read" =
    let ti1 = (match (currentToken, rest, cline) "read") in
    let ti2 = (match ti1 "(") in
    let (ti3, code1, table1, fn1, indfn1) = (expression ti2 nest table fn) in
    let ti4 = (match ti3 ")") in
    let ti5 = (match ti4 ";") in
    (ti5, "\tread(" ++ code1 ++ ");\n", table1, fn1 ++ indfn1)
  | otherwise = (reportError (predict "read_func_call") (show cline) currentToken)

-- write_func_call -> write ( expression ) ;
write_func_call :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
write_func_call (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "write" =
    let ti1 = (match (currentToken, rest, cline) "write") in
    let ti2 = (match ti1 "(") in
    let (ti3, code1, table1, fn1, indfn1) = (expression ti2 nest table fn) in
    let ti4 = (match ti3 ")") in
    let ti5 = (match ti4 ";") in
    (ti5, "\twrite(" ++ code1 ++ ");\n", table1, fn1 ++ indfn1)
  | otherwise = (reportError (predict "write_func_call") (show cline) currentToken)

-- expr_list -> ε | non_empty_expr_list  
expr_list :: TokenIterator -> Int -> Table -> String -> (TokenIterator, [String], [String], Table, String)
expr_list (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) `elem` (parseTable "expr_list" "FOLLOW") =
    ((currentToken, rest, cline), [], [], table, fn)
  | (tokenType currentToken) `elem` (predict "non_empty_expr_list") =
    let (ti1, varList1, indfnList1, table1, fn1) = (non_empty_expr_list (currentToken, rest, cline) nest table fn) in
    (ti1, varList1, indfnList1, table1, fn1)
  | otherwise = (reportError (predict "expr_list") (show cline) currentToken)
  
-- non_empty_expr_list -> expression non_empty_expr_list_tail
non_empty_expr_list :: TokenIterator -> Int -> Table -> String -> (TokenIterator, [String], [String], Table, String)
non_empty_expr_list (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) `elem` (predict "expression") =
    let (ti1, code1, table1, fn1, indfn1) = (expression (currentToken, rest, cline) nest table fn) in
    let (ti2, varList1, indfnList1, table2, fn2) = (non_empty_expr_list_tail ti1 nest table1 fn1) in
    (ti2, code1 : varList1, indfn1 : indfnList1, table2, fn2)
  | otherwise = (reportError (predict "non_empty_expr_list") (show cline) currentToken)
  
-- non_empty_expr_list_tail -> ε | , expression non_empty_expr_list_tail
non_empty_expr_list_tail :: TokenIterator -> Int -> Table -> String -> (TokenIterator, [String], [String], Table, String)
non_empty_expr_list_tail (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let (ti2, code1, table1, fn1, indfn1) = (expression ti1 nest table fn) in
    let (ti3, varList1, indfnList1, table2, fn2) = (non_empty_expr_list_tail ti2 nest table1 fn1) in
    (ti3, code1 : varList1, indfn1 : indfnList1, table2, fn2)
  | (tokenType currentToken) `elem` (parseTable "non_empty_expr_list_tail" "FOLLOW") =
    ((currentToken, rest, cline), [], [], table, fn)
  | otherwise = (reportError (predict "non_empty_expr_list_tail") (show cline) currentToken)
  
-- if_statement -> if ( condition_expression ) block_statements if_statement_tail
if_statement :: TokenIterator -> Int -> Table -> String -> [String] -> (TokenIterator, String, Table, String)
if_statement (currentToken, rest, cline) nest table fn bc_label
  | (tokenType currentToken) == "if" =
    let ti1 = (match (currentToken, rest, cline) "if") in
    let ti2 = (match ti1 "(") in
    let (ti3, table1, fn1, true_label, false_label) = (condition_expression ti2 nest table fn) in
    let ti4 = (match ti3 ")") in
    let (ti5, code1, table2) = (block_statements ti4 nest table1 bc_label) in
    let (ti6, code2, table3) = (if_statement_tail ti5 nest table2 false_label bc_label) in
    (ti6, true_label ++ ":\n\t;\n" ++ code1 ++ code2, table3, fn1)
  | otherwise = (reportError (predict "if_statement") (show cline) currentToken)
  
-- if_statement_tail -> ε | else block_statements
if_statement_tail :: TokenIterator -> Int -> Table -> String -> [String] -> (TokenIterator, String, Table)
if_statement_tail (currentToken, rest, cline) nest table false_label bc_label
  | (tokenType currentToken) == "else" =
    let ti1 = (match (currentToken, rest, cline) "else") in
    let (ti2, code1, table1) = (block_statements ti1 nest table bc_label) in
    let (table2, else_label) = (getConditionalLabel table1) in
    (ti2, "\tgoto " ++ else_label ++ ";\n" ++ false_label ++ ":\n\t;\n" ++ code1 ++ else_label ++ ":\n\t;\n", table2)
  | (tokenType currentToken) `elem` (parseTable "if_statement_tail" "FOLLOW") =
    ((currentToken, rest, cline), false_label ++ ":\n\t;\n", table)
  | otherwise = (reportError (predict "if_statement_tail") (show cline) currentToken)
  
-- condition_expression -> condition condition_expression_tail
condition_expression :: TokenIterator -> Int -> Table -> String -> (TokenIterator, Table, String, String, String)
condition_expression (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) `elem` (predict "condition") =
    let (table1, true_label) = (getConditionalLabel table) in
    let (table2, false_label) = (getConditionalLabel table1) in
    let (ti1, code1, table3, fn1) = (condition (currentToken, rest, cline) nest table2 fn) in
    let (ti2, table4, fn2) = (condition_expression_tail ti1 nest table3 fn1 code1 true_label false_label) in
    (ti2, table4, fn2, true_label, false_label)
  | otherwise = (reportError (predict "condition_expression") (show cline) currentToken)

-- condition_expression_tail -> ε | condition_op condition
condition_expression_tail :: TokenIterator -> Int -> Table -> String -> String -> String -> String -> (TokenIterator, Table, String)
condition_expression_tail (currentToken, rest, cline) nest table fn condition_left true_label false_label
  | (tokenType currentToken) `elem` (predict "condition_op") =
    let (ti1, code1) = (condition_op (currentToken, rest, cline)) in 
    let (table1, fn1) = (conditionCode table code1 fn condition_left true_label false_label)
         where conditionCode table condition_op_code fn condition_left true_label false_label
                | condition_op_code == "||" = (table, fn ++ "\tif(" ++ condition_left ++ ") goto " ++ true_label ++ ";\n")
                | condition_op_code == "&&" = (table1, fn ++ "\tif(" ++ condition_left ++ ") goto " ++ sc_label ++ ";\n" ++
                                              "\tgoto " ++ false_label ++ ";\n" ++ sc_label ++ ":\n\t;\n")
                                                where (table1, sc_label) = (getConditionalLabel table) in
    let (ti2, code2, table2, fn2) = (condition ti1 nest table1 fn1) in
    (ti2, table2, fn2 ++ "\tif(" ++ code2 ++ ")" ++ " goto " ++ true_label ++ ";\n" ++
    "\tgoto " ++ false_label ++ ";\n")
  | (tokenType currentToken) `elem` (parseTable "condition_expression_tail" "FOLLOW") =
    ((currentToken, rest, cline), table, fn ++ 
    "\tif(" ++ condition_left ++ ") goto "  ++ true_label ++ ";\n" ++
    "\tgoto " ++ false_label ++ ";\n")
  | otherwise = (reportError (predict "condition_expression_tail") (show cline) currentToken)
  
-- condition_op -> && | ||
condition_op :: TokenIterator -> (TokenIterator, String)
condition_op (currentToken, rest, cline)
  | (tokenType currentToken) == "&&" =
    let ti1 = (match (currentToken, rest, cline) "&&") in
    (ti1, "&&")
  | (tokenType currentToken) == "||" =
    let ti1 = (match (currentToken, rest, cline) "||") in
    (ti1, "||")
  | otherwise = (reportError (predict "condition_op") (show cline) currentToken)
  
-- condition -> expression comparison_op expression
condition :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
condition (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) `elem` (predict "expression") =
    let (ti1, code1, table1, fn1, indfn1) = (expression (currentToken, rest, cline) nest table fn) in
    let (ti2, code2) = (comparison_op ti1) in 
    let (ti3, code3, table2, fn2, indfn2) = (expression ti2 nest table1 fn1) in
    (ti3, "r1" ++ code2 ++ "r2", table2, fn2 ++ indfn1 ++ "\tr1 = " ++ code1 ++ ";\n" ++ indfn2 ++ "\tr2 = " ++ code3 ++ ";\n")
  | otherwise = (reportError (predict "condition") (show cline) currentToken)
  
-- comparison_op -> == | != | > | >= | < | <= 
comparison_op :: TokenIterator -> (TokenIterator, String)
comparison_op (currentToken, rest, cline)
  | (tokenType currentToken) == "==" =
    let ti1 = (match (currentToken, rest, cline) "==") in
    (ti1, " == ")
  | (tokenType currentToken) == "!=" =
    let ti1 = (match (currentToken, rest, cline) "!=") in
    (ti1, " != ")
  | (tokenType currentToken) == ">" =
    let ti1 = (match (currentToken, rest, cline) ">") in
    (ti1, " > ")
  | (tokenType currentToken) == ">=" =
    let ti1 = (match (currentToken, rest, cline) ">=") in
    (ti1, " >= ")
  | (tokenType currentToken) == "<" =
    let ti1 = (match (currentToken, rest, cline) "<") in
    (ti1, " < ")
  | (tokenType currentToken) == "<=" =
    let ti1 = (match (currentToken, rest, cline) "<=") in
    (ti1, " <= ")
  | otherwise = (reportError (predict "comparison_op") (show cline) currentToken)
  
-- while_statement -> while ( condition_expression ) block_statements	
while_statement :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
while_statement (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "while" =
    let ti1 = (match (currentToken, rest, cline) "while") in
    let ti2 = (match ti1 "(") in
    let (table1, begin_label) = (getConditionalLabel table) in
    let fn1 = (fn ++ begin_label ++ ":\n\t;\n") in
    let (ti3, table2, fn2, true_label, false_label) = (condition_expression ti2 nest table1 fn1) in
    let ti4 = (match ti3 ")") in
    let (ti5, code1, table3) = (block_statements ti4 nest table2 [begin_label, false_label]) in
    (ti5, true_label ++ ":\n\t;\n" ++ code1 ++ "\tgoto " ++ begin_label ++ ";\n" ++ false_label ++ ":\n\t;\n" , table3, fn2)
  | otherwise = (reportError (predict "while_statement") (show cline) currentToken)

-- return_statement -> return return_statement_tail
return_statement :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
return_statement (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "return" =
    let ti1 = (match (currentToken, rest, cline) "return") in
    let (ti2, code1, table1, fn1) = (return_statement_tail ti1 nest table fn) in
    (ti2, code1, table1, fn1)
  | otherwise = (reportError (predict "return_statement") (show cline) currentToken)

-- return_statement_tail -> expression ; | ;
return_statement_tail :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String)
return_statement_tail (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) `elem` (predict "expression") =
    let (ti1, code1, table1, fn1, indfn1) = (expression (currentToken, rest, cline) nest table fn) in
    let ti2 = (match ti1 ";") in
    (ti2, epilogue (Just code1), table1, fn1 ++ indfn1)
  | (tokenType currentToken) == ";" =
    let ti1 = (match (currentToken, rest, cline) ";") in
    (ti1, epilogue Nothing, table, fn)
  | otherwise = (reportError (predict "return_statement_tail") (show cline) currentToken)
  
-- break_statement -> break ;
break_statement :: TokenIterator -> Table -> [String] -> (TokenIterator, String, Table)
break_statement (currentToken, rest, cline) table label
  | currentToken == "break" =
    let ti1 = (match (currentToken, rest, cline) "break") in
    let ti2 = (match ti1 ";") in
    let code1 = (breakCode label)
         where breakCode [] = "\n"
               breakCode label = "\tgoto " ++ (label !! 1) ++ ";\n" in
    (ti2, code1, table)
  | otherwise = (reportError (predict "break_statement") (show cline) currentToken)
  
-- continue_statement -> continue ;
continue_statement :: TokenIterator -> Table -> [String] -> (TokenIterator, String, Table)
continue_statement (currentToken, rest, cline) table label
  | currentToken == "continue" =
    let ti1 = (match (currentToken, rest, cline) "continue") in
    let ti2 = (match ti1 ";") in
    let code1 = (continueCode label)
         where continueCode [] = "\n"
               continueCode label = "\tgoto " ++ (label !! 0) ++ ";\n" in
    (ti2, code1, table)
  | otherwise = (reportError (predict "continue_statement") (show cline) currentToken)

-- expression -> term expression_tail
expression :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String, String)
expression (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) `elem` (predict "term") =
    let (ti1, code1, table1, fn1, indfn1) = (term (currentToken, rest, cline) nest table fn) in
    let (ti2, code2, table2, fn2, indfn2) = (expression_tail ti1 nest table1 fn1 code1 indfn1) in
    (ti2, code2, table2, fn2, indfn2)
  | otherwise = (reportError (predict "expression") (show cline) currentToken)

-- expression_tail -> ε | addop term expression_tail
expression_tail :: TokenIterator -> Int -> Table -> String -> String -> String -> (TokenIterator, String, Table, String, String)
expression_tail (currentToken, rest, cline) nest table fn et_left indfn
  | (tokenType currentToken) `elem` (predict "addop") =
    let (ti1, code1) = (addop (currentToken, rest, cline)) in
    let (ti2, code2, table1, fn1, indfn1) = (term ti1 nest table fn) in
    let (table2, et_place) = (addTemp table1 nest) in
    let regCode = fn1 ++ indfn ++
                  "\tr1 = " ++ et_left ++ ";\n" ++
                  indfn1 ++
                  "\tr2 = " ++ code2 ++ ";\n" ++
                  "\tr1 = r1" ++ code1 ++ "r2;\n" ++
                  (growStackFromTemp table2 nest) ++
                  "\t" ++ et_place ++ " = r1;\n" in
    let fn2 = regCode in
    let (ti3, code3, table3, fn3, indfn2) = (expression_tail ti2 nest table2 fn2 et_place "") in
    (ti3, code3, table3, fn3, indfn2)
  | (tokenType currentToken) `elem` (parseTable "expression_tail" "FOLLOW") =
    ((currentToken, rest, cline), et_left, table, fn, indfn)
  | otherwise = (reportError (predict "expression_tail") (show cline) currentToken)

-- addop -> + | -
addop :: TokenIterator -> (TokenIterator, String)
addop (currentToken, rest, cline)
  | (tokenType currentToken) == "+" =
    let ti1 = (match (currentToken, rest, cline) "+") in
    (ti1, " + ")
  | (tokenType currentToken) == "-" =
    let ti1 = (match (currentToken, rest, cline) "-") in
    (ti1, " - ")
  | otherwise = (reportError (predict "addop") (show cline) currentToken)
  
-- term -> factor term_tail
term :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String, String)
term (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) `elem` (predict "factor") =
    let (ti1, code1, table1, fn1, indfn1) = (factor (currentToken, rest, cline) nest table fn) in
    let (ti2, code2, table2, fn2, indfn2) = (term_tail ti1 nest table1 fn1 code1 indfn1) in
    (ti2, code2, table2, fn2, indfn2)
  | otherwise = (reportError (predict "term") (show cline) currentToken)
  
-- term_tail -> ε | mulop factor term_tail
term_tail :: TokenIterator -> Int -> Table -> String -> String -> String -> (TokenIterator, String, Table, String, String)
term_tail (currentToken, rest, cline) nest table fn tt_left indfn
  | (tokenType currentToken) `elem` (predict "mulop") =
    let (ti1, code1) = (mulop (currentToken, rest, cline)) in
    let (ti2, code2, table1, fn1, indfn1) = (factor ti1 nest table fn) in
    let (table2, tt_place) = (addTemp table1 nest) in
    let regCode = fn1 ++ indfn ++
                  "\tr1 = " ++ tt_left ++ ";\n" ++
                  indfn1 ++
                  "\tr2 = " ++ code2 ++ ";\n" ++
                  "\tr1 = r1" ++ code1 ++ "r2;\n" ++
                  (growStackFromTemp table2 nest) ++
                  "\t" ++ tt_place ++ " = r1;\n" in
    let fn2 = regCode in
    let (ti3, code3, table3, fn3, indfn2) = (term_tail ti2 nest table2 fn2 tt_place "") in
    (ti3, code3, table3, fn3, indfn2)
  | (tokenType currentToken) `elem` (parseTable "term_tail" "FOLLOW") =
    ((currentToken, rest, cline), tt_left, table, fn, indfn)
  | otherwise = (reportError (predict "term_tail") (show cline) currentToken)
  
-- mulop -> * | /
mulop :: TokenIterator -> (TokenIterator, String)
mulop (currentToken, rest, cline)
  | (tokenType currentToken) == "*" =
    let ti1 = (match (currentToken, rest, cline) "*") in
    (ti1, " * ")
  | (tokenType currentToken) == "/" =
    let ti1 = (match (currentToken, rest, cline) "/") in
    (ti1, " / ")
  | otherwise = (reportError (predict "mulop") (show cline) currentToken)
  
-- factor -> identifier factor_tail | number | - number | ( expression )  
factor :: TokenIterator -> Int -> Table -> String -> (TokenIterator, String, Table, String, String)
factor (currentToken, rest, cline) nest table fn
  | (tokenType currentToken) == "identifier" =
    let ti1 = (match (currentToken, rest, cline) "identifier") in
    let (ti2, code1, table1, fn1, indfn1) = (factor_tail ti1 nest table fn (extractValue currentToken)) in
    (ti2, code1, table1, fn1, indfn1)
  | (tokenType currentToken) == "number" =
    let ti1 = (match (currentToken, rest, cline) "number") in
    let (table1, named_var) = (addTemp table nest) in
    let fn1 = (fn ++ (growStackFromTemp table1 nest) ++ "\t" ++ named_var ++ " = " ++ (extractValue currentToken) ++ ";\n") in
    (ti1, named_var, table1, fn1, "")
  | (tokenType currentToken) == "-" =
    let ti1 = (match (currentToken, rest, cline) "-") in
    let ti2 = (match ti1 "number") in
    let (table1, named_var) = (addTemp table nest) in
    let fn1 = (fn ++ (growStackFromTemp table1 nest) ++ "\t" ++ named_var ++ " = " ++ "-" ++ (extractValue (tiHead ti1)) ++ ";\n") in
    (ti2, named_var, table1, fn1, "")
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (ti2, code1, table1, fn1, indfn1) = (expression ti1 nest table fn) in
    let ti3 = (match ti2 ")") in
    (ti3, code1, table1, fn1, indfn1)
  | otherwise = (reportError (predict "factor") (show cline) currentToken)
  
-- factor_tail -> ε | [expression] | ( expr_list )
factor_tail :: TokenIterator -> Int -> Table -> String -> String -> (TokenIterator, String, Table, String, String)
factor_tail (currentToken, rest, cline) nest table fn idenValue
  | (tokenType currentToken) `elem` (parseTable "factor_tail" "FOLLOW") =
    let (table1, named_var, indfn1) = (getMemVar table nest idenValue (show cline)) in
    ((currentToken, rest, cline), named_var, table1, fn, indfn1)
  | (tokenType currentToken) == "[" =
    let ti1 = (match (currentToken, rest, cline) "[") in
    let (ti2, code1, table1, fn1, indfn1) = (expression ti1 nest table fn) in
    let ti3 = (match ti2 "]") in
    let (table2, named_var1) = (addTemp table1 nest) in
    let (table3, named_var2, indfn2) = (getMemArrayVar table2 nest idenValue named_var1 (show cline)) in
    (ti3, named_var2, table3, fn1 ++ (growStackFromTemp table3 nest), indfn1 ++ "\t" ++ named_var1 ++ " = " ++ code1 ++ ";\n" ++ indfn2)
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (ti2, varList1, indfnList1, table1, fn1) = (expr_list ti1 nest table fn) in
    let ti3 = (match ti2 ")") in
    let (table2, named_var) = (addTemp table1 nest) in
    let (table3, retLabel) = (getReturnLabel table2) in
    let nest1 = (getScopeCount table3 idenValue) in
    let code1 = ((preJump nest1 retLabel varList1 indfnList1) ++ "\tgoto _func_" ++ idenValue ++ ";\n" ++ (postJump table3 nest retLabel (Just named_var))) in
    let fn2 = (fn1 ++ (growStackFromTemp table3 nest) ++ code1) in
    (ti3, named_var, table3, fn2, "")
  | otherwise = (reportError (predict "factor_tail") (show cline) currentToken)

-- ghc --make translator
main = do
  (inputFile : _) <- getArgs
  tokens <- scan inputFile
-- Init Symbol table
  let symbolTables = Table 1 1 (Map.empty) (Map.singleton 0 (-1, Counts 0 Map.empty, Counts 0 Map.empty))
-- Start parsing
  let (_, programCode, _) = program (nextToken tokens 1) symbolTables
-- Get meta-statements
  let metaStatements = unlines (getMetaStatements tokens)
-- Print comments
  putStrLn metaStatements
-- Print generated code 
  putStrLn programCode

