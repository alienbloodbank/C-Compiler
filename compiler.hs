import Scanner
import ParseTable
import SymbolTable
import ProgramLayout

import System.Exit
import System.Environment

{-
Parser/Translator
Author: Soubhk Ghosh
11/19/2018
-}

getComments :: [Token] -> [String]
getComments [] = []
getComments ((Comment c) : rest) = c : getComments rest
getComments (_ : rest) = getComments rest

type TokenIterator = (String, [Token], Int)

-- Token iterator
nextToken :: [Token] -> Int -> TokenIterator
nextToken [] cline = ("EOF", [], cline)
nextToken ((Comment c) : rest) cline = nextToken rest (cline+1)
nextToken ((Reserved r) : rest) cline = (r, rest, cline)
nextToken ((Symbol "\n") : rest) cline = nextToken rest (cline+1)
nextToken ((Symbol s) : rest) cline = (s, rest, cline)
nextToken ((Number s) : rest) cline = ("number " ++ s, rest, cline)
nextToken ((String s) : rest) cline = ("string " ++ s, rest, cline)
nextToken ((Identifier s) : rest) cline = ("identifier " ++ s, rest, cline)

extractValue :: String -> String
extractValue s = snd (splitAt 1 (dropWhile (/=' ') s))

tiHead :: TokenIterator -> String
tiHead (ti, tokens, cline) = ti

tokenType :: String -> String
tokenType = takeWhile (/=' ')

-- Token consumer
match :: TokenIterator -> String -> TokenIterator
match (currentToken, rest, cline) expected
  | (tokenType currentToken) == expected = nextToken rest cline
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": expected " ++ expected)

-- program -> program_data EOF
program :: TokenIterator -> Table -> (TokenIterator, String, Table)
program (currentToken, rest, cline) table
  | (tokenType currentToken) `elem` (predict "program_data") = 
    let (ti1, code1, table1) = (program_data (currentToken, rest, cline) table) in
    let ti2 = (match ti1 "EOF") in
    (ti2, (vonNeumannCode table1 code1), table1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- program_data -> ε | global_decl program_data
program_data :: TokenIterator -> Table -> (TokenIterator, String, Table)
program_data (currentToken, rest, cline) table
  | (tokenType currentToken) `elem` (parseTable "program_data" "FOLLOW") =
    ((currentToken, rest, cline), "", table)
  | (tokenType currentToken) `elem` (predict "global_decl") =
    let (ti1, code1, table1) = (global_decl (currentToken, rest, cline) table) in
    let (ti2, code2, table2) = (program_data ti1 table1) in
    (ti2, code1 ++ code2, table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- global_decl -> type_name identifier global_decl_tail
global_decl :: TokenIterator -> Table -> (TokenIterator, String, Table)
global_decl (currentToken, rest, cline) table
  | (tokenType currentToken) `elem` (predict "type_name") =
    let ti1 = (type_name (currentToken, rest, cline)) in
    let ti2 = (match ti1 "identifier") in
    let (ti3, code1, table1) = (global_decl_tail ti2 table (extractValue (tiHead ti1))) in
    (ti3, code1, table1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": Invalid declaration.")

-- global_decl_tail -> id_list_tail ; | ( parameter_list ) func_tail
global_decl_tail :: TokenIterator -> Table -> String -> (TokenIterator, String, Table)
global_decl_tail (currentToken, rest, cline) table idenValue
  | (tokenType currentToken) `elem` (predict "id_list_tail") =
    let table1 = (addEntry table "global" idenValue) in
    let (ti1, table2) = (id_list_tail (currentToken, rest, cline) table1 "global") in
    let ti2 = (match ti1 ";") in
    (ti2, "", table2)
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (Table lc1 lc2 (Temps (cs1, _, _))) = table in
    let table1 = (Table lc1 lc2 (Temps (cs1, Counts 0 [], Counts 0 []))) in 
    let (ti2, table2) = (parameter_list ti1 table1) in
    let ti3 = (match ti2 ")") in
    let (ti4, code1, table3) = (func_tail ti3 table2 idenValue) in
    (ti4, code1, table3)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- func_tail -> ; | { data_decls statements }
func_tail :: TokenIterator -> Table -> String -> (TokenIterator, String, Table)
func_tail (currentToken, rest, cline) table idenValue
  | (tokenType currentToken) == ";" =
    let ti1 = (match (currentToken, rest, cline) ";") in
    (ti1, "\n", table)
  | (tokenType currentToken) == "{" =
    let ti1 = (match (currentToken, rest, cline) "{") in
    let (ti2, table1) = (data_decls ti1 table) in
    let (ti3, code1, table2) = (statements ti2 table1 []) in
    let ti4 = (match ti3 "}") in
    (ti4, (prologue table2 idenValue) ++ (funcTailCode table2 code1), table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\". Expecting \";\" or \"{\"")
  
-- type_name -> int | void
type_name :: TokenIterator -> TokenIterator
type_name (currentToken, rest, cline)
  | (tokenType currentToken) == "int" =
    let ti1 = (match (currentToken, rest, cline) "int") in
    ti1
  | (tokenType currentToken) == "void" =
    let ti1 = (match (currentToken, rest, cline) "void") in
    ti1
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid type name at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\". Expecting \"int\" or \"void\"")
  
-- parameter_list -> ε | void parameter_list_tail | int identifier non_empty_parameter_list  
parameter_list :: TokenIterator -> Table -> (TokenIterator, Table)
parameter_list (currentToken, rest, cline) table
  | (tokenType currentToken) `elem` (parseTable "parameter_list" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | (tokenType currentToken) == "void" =
    let ti1 = (match (currentToken, rest, cline) "void") in
    let (ti2, table1) = (parameter_list_tail ti1 table "void") in
    (ti2, table1)
  | (tokenType currentToken) == "int" =
    let ti1 = (match (currentToken, rest, cline) "int") in
    let ti2 = (match ti1 "identifier") in
    let table1 = (addEntry table "param" (extractValue (tiHead ti1))) in
    let (ti3, table2) = (non_empty_parameter_list ti2 table1) in
    (ti3, table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid parameters at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- parameter_list_tail -> ε | identifier non_empty_parameter_list
parameter_list_tail :: TokenIterator -> Table -> String -> (TokenIterator, Table)
parameter_list_tail (currentToken, rest, cline) table typeNameCode
  | (tokenType currentToken) `elem` (parseTable "parameter_list_tail" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | (tokenType currentToken) == "identifier" =
    let ti1 = (match (currentToken, rest, cline) "identifier") in
    let table1 = (addEntry table "param" (extractValue currentToken)) in
    let (ti2, table2) = (non_empty_parameter_list ti1 table1) in
    (ti2, table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid parameters at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- non_empty_parameter_list -> ε | , type_name identifier non_empty_parameter_list  
non_empty_parameter_list :: TokenIterator -> Table -> (TokenIterator, Table)
non_empty_parameter_list (currentToken, rest, cline) table
  | (tokenType currentToken) `elem` (parseTable "non_empty_parameter_list" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let ti2 = (type_name ti1) in
    let ti3 = (match ti2 "identifier") in
    let table1 = (addEntry table "param" (extractValue (tiHead ti2))) in
    let (ti4, table2) = (non_empty_parameter_list ti3 table1) in
    (ti3, table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid parameters at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- data_decls -> ε | type_name id_list ; data_decls
data_decls :: TokenIterator -> Table -> (TokenIterator, Table)
data_decls (currentToken, rest, cline) table
  | (tokenType currentToken) `elem` (parseTable "data_decls" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | (tokenType currentToken) `elem` (predict "type_name") =
    let ti1 = (type_name (currentToken, rest, cline)) in
    let (ti2, table1) = (id_list ti1 table) in
    let ti3 = (match ti2 ";") in
    let (ti4, table2) = (data_decls ti3 table1) in
    (ti4, table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- id_list -> identifier id_list_tail
id_list :: TokenIterator -> Table -> (TokenIterator, Table)
id_list (currentToken, rest, cline) table
  | (tokenType currentToken) == "identifier" =
    let ti1 = (match (currentToken, rest, cline) "identifier") in
    let table1 = (addEntry table "local" (extractValue currentToken)) in
    let (ti2, table2) = (id_list_tail ti1 table1 "local") in
    (ti2, table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid identifiers at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- id_list_tail -> ε | , identifier id_list_tail
id_list_tail :: TokenIterator -> Table -> String -> (TokenIterator, Table)
id_list_tail (currentToken, rest, cline) table scope
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let ti2 = (match ti1 "identifier") in
    let table1 = (addEntry table scope (extractValue (tiHead ti1))) in
    let (ti3, table2) = (id_list_tail ti2 table1 scope) in
    (ti3, table2)
  | (tokenType currentToken) `elem` (parseTable "id_list_tail" "FOLLOW") =
    ((currentToken, rest, cline), table)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid identifiers at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- block_statements -> { statements }
block_statements :: TokenIterator -> Table -> [String] -> (TokenIterator, String, Table)
block_statements (currentToken, rest, cline) table bc_label
  | (tokenType currentToken) == "{" =
    let ti1 = (match (currentToken, rest, cline) "{") in
    let (ti2, code1, table1) = (statements ti1 table bc_label) in
    let ti3 = (match ti2 "}") in
    (ti3, code1, table1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid statements at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- statements -> ε | statement statements
statements :: TokenIterator -> Table -> [String] -> (TokenIterator, String, Table)
statements (currentToken, rest, cline) table bc_label
  | (tokenType currentToken) `elem` (parseTable "statements" "FOLLOW") =
    ((currentToken, rest, cline), "", table)
  | (tokenType currentToken) `elem` (predict "statement") =
    let (ti1, code1, table1, fn1) = (statement (currentToken, rest, cline) table "" bc_label) in
    let (ti2, code2, table2) = (statements ti1 table1 bc_label) in
    (ti2, fn1 ++ code1 ++ code2, table2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid statements at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
{-
statement -> assignment_or_general_func_call | 
			printf_func_call | 
			scanf_func_call | 
			if_statement | 
			while_statement	| 
			return_statement | 
			break_statement | 
			continue_statement  
-}
statement :: TokenIterator -> Table -> String -> [String] -> (TokenIterator, String, Table, String)
statement (currentToken, rest, cline) table fn bc_label
  | (tokenType currentToken) `elem` (predict "assignment_or_general_func_call") =
    let (ti1, code1, table1, fn1) = (assignment_or_general_func_call (currentToken, rest, cline) table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "printf_func_call") =
    let (ti1, code1, table1, fn1) = (printf_func_call (currentToken, rest, cline) table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "scanf_func_call") =
    let (ti1, code1, table1, fn1) = (scanf_func_call (currentToken, rest, cline) table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "if_statement") =
    let (ti1, code1, table1, fn1) = (if_statement (currentToken, rest, cline) table fn bc_label) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "while_statement") =
    let (ti1, code1, table1, fn1) = (while_statement (currentToken, rest, cline) table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "return_statement") =
    let (ti1, code1, table1, fn1) = (return_statement (currentToken, rest, cline) table fn) in
    (ti1, code1, table1, fn1)
  | (tokenType currentToken) `elem` (predict "break_statement") =
    let (ti1, code1, table1) = (break_statement (currentToken, rest, cline) table bc_label) in
    (ti1, code1, table1, fn)
  | (tokenType currentToken) `elem` (predict "continue_statement") =
    let (ti1, code1, table1) = (continue_statement (currentToken, rest, cline) table bc_label) in
    (ti1, code1, table1, fn)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- assignment_or_general_func_call -> identifier assignment_or_general_func_call_tail
assignment_or_general_func_call :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
assignment_or_general_func_call (currentToken, rest, cline) table fn
  | (tokenType currentToken) == "identifier" =
    let ti1 = (match (currentToken, rest, cline) "identifier") in
    let (ti2, code1, table1, fn1) = (assignment_or_general_func_call_tail ti1 table fn (extractValue currentToken)) in
    (ti2, code1, table1, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- assignment_or_general_func_call_tail -> = expression ; | ( expr_list ) ;
assignment_or_general_func_call_tail :: TokenIterator -> Table -> String -> String -> (TokenIterator, String, Table, String)
assignment_or_general_func_call_tail (currentToken, rest, cline) table fn idenValue
  | (tokenType currentToken) == "=" =
    let ti1 = (match (currentToken, rest, cline) "=") in
    let (ti2, code1, table1, fn1) = (expression ti1 table fn) in
    let ti3 = (match ti2 ";") in
    let named_var = (getMemVar table1 idenValue) in
    (ti3, named_var ++ " = " ++ code1 ++ ";\n", table1, fn1)
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (ti2, varList1, table1, fn1) = (expr_list ti1 table fn) in
    let ti3 = (match ti2 ")") in
    let ti4 = (match ti3 ";") in
    let (table2, retLabel) = (getReturnLabel table1) in  
    (ti4, (preJump retLabel varList1) ++ "goto " ++ idenValue ++ "Func;\n"  ++ (postJump retLabel Nothing), table2, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\". Expecting \"=\" or \"(\"")

-- printf_func_call -> printf ( STRING printf_func_call_tail   
printf_func_call :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
printf_func_call (currentToken, rest, cline) table fn
  | (tokenType currentToken) == "printf" =
    let ti1 = (match (currentToken, rest, cline) "printf") in
    let ti2 = (match ti1 "(") in
    let ti3 = (match ti2 "string") in
    let (ti4, code1, table1, fn1) = (printf_func_call_tail ti3 table fn) in
    (ti4, "printf(" ++ (extractValue (tiHead ti2)) ++ code1, table1, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid printf call at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- printf_func_call_tail -> ) ; | , expression ) ;
printf_func_call_tail :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
printf_func_call_tail (currentToken, rest, cline) table fn
  | (tokenType currentToken) == ")" =
    let ti1 = (match (currentToken, rest, cline) ")") in
    let ti2 = (match ti1 ";") in
    (ti2, ");\n", table, fn)
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let (ti2, code1, table1, fn1) = (expression ti1 table fn) in
    let ti3 = (match ti2 ")") in
    let ti4 = (match ti3 ";") in
    (ti4, ", " ++ code1 ++ ");\n", table1, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\". Expecting \")\" or \",\"") 

-- scanf_func_call -> scanf ( string , &expression ) ;
scanf_func_call :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
scanf_func_call (currentToken, rest, cline) table fn
  | (tokenType currentToken) == "scanf" =
    let ti1 = (match (currentToken, rest, cline) "scanf") in
    let ti2 = (match ti1 "(") in
    let ti3 = (match ti2 "string") in
    let ti4 = (match ti3 ",") in
    let ti5 = (match ti4 "&") in
    let (ti6, code1, table1, fn1) = (expression ti5 table fn) in
    let ti7 = (match ti6 ")") in
    let ti8 = (match ti7 ";") in
    (ti8, "scanf(" ++ (extractValue (tiHead ti2)) ++ ", &" ++ code1 ++ ");\n", table1, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid scanf call at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- expr_list -> ε | non_empty_expr_list  
expr_list :: TokenIterator -> Table -> String -> (TokenIterator, [String], Table, String)
expr_list (currentToken, rest, cline) table fn
  | (tokenType currentToken) `elem` (parseTable "expr_list" "FOLLOW") =
    ((currentToken, rest, cline), [], table, fn)
  | (tokenType currentToken) `elem` (predict "non_empty_expr_list") =
    let (ti1, varList1, table1, fn1) = (non_empty_expr_list (currentToken, rest, cline) table fn) in
    (ti1, varList1, table1, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- non_empty_expr_list -> expression non_empty_expr_list_tail
non_empty_expr_list :: TokenIterator -> Table -> String -> (TokenIterator, [String], Table, String)
non_empty_expr_list (currentToken, rest, cline) table fn
  | (tokenType currentToken) `elem` (predict "expression") =
    let (ti1, code1, table1, fn1) = (expression (currentToken, rest, cline) table fn) in
    let (ti2, varList1, table2, fn2) = (non_empty_expr_list_tail ti1 table1 fn1) in
    (ti2, code1 : varList1, table2, fn2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- non_empty_expr_list_tail -> ε | , expression non_empty_expr_list_tail
non_empty_expr_list_tail :: TokenIterator -> Table -> String -> (TokenIterator, [String], Table, String)
non_empty_expr_list_tail (currentToken, rest, cline) table fn
  | (tokenType currentToken) == "," =
    let ti1 = (match (currentToken, rest, cline) ",") in
    let (ti2, code1, table1, fn1) = (expression ti1 table fn) in
    let (ti3, varList1, table2, fn2) = (non_empty_expr_list_tail ti2 table1 fn1) in
    (ti3, code1 : varList1, table2, fn2)
  | (tokenType currentToken) `elem` (parseTable "non_empty_expr_list_tail" "FOLLOW") =
    ((currentToken, rest, cline), [], table, fn)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- if_statement -> if ( condition_expression ) block_statements if_statement_tail
if_statement :: TokenIterator -> Table -> String -> [String] -> (TokenIterator, String, Table, String)
if_statement (currentToken, rest, cline) table fn bc_label
  | (tokenType currentToken) == "if" =
    let ti1 = (match (currentToken, rest, cline) "if") in
    let ti2 = (match ti1 "(") in
    let (ti3, table1, fn1, true_label, false_label) = (condition_expression ti2 table fn) in 
    let ti4 = (match ti3 ")") in
    let (ti5, code1, table2) = (block_statements ti4 table1 bc_label) in
    let (ti6, code2, table3) = (if_statement_tail ti5 table2 false_label bc_label) in
    (ti6, true_label ++ ":;\n" ++ code1 ++ code2, table3, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid if statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- if_statement_tail -> ε | else block_statements
if_statement_tail :: TokenIterator -> Table -> String -> [String] -> (TokenIterator, String, Table)
if_statement_tail (currentToken, rest, cline) table false_label bc_label
  | (tokenType currentToken) == "else" =
    let ti1 = (match (currentToken, rest, cline) "else") in
    let (ti2, code1, table1) = (block_statements ti1 table bc_label) in
    let (table2, else_label) = (getConditionalLabel table1) in
    (ti2, "goto " ++ else_label ++ ";\n" ++ false_label ++ ":;\n" ++ code1 ++ else_label ++ ":;\n", table2)
  | (tokenType currentToken) `elem` (parseTable "if_statement_tail" "FOLLOW") =
    ((currentToken, rest, cline), false_label ++ ":;\n", table)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid if statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- condition_expression -> condition condition_expression_tail
condition_expression :: TokenIterator -> Table -> String -> (TokenIterator, Table, String, String, String)
condition_expression (currentToken, rest, cline) table fn
  | (tokenType currentToken) `elem` (predict "condition") =
    let (table1, true_label) = (getConditionalLabel table) in
    let (table2, false_label) = (getConditionalLabel table1) in
    let (ti1, code1, table3, fn1) = (condition (currentToken, rest, cline) table2 fn) in
    let (ti2, table4, fn2) = (condition_expression_tail ti1 table3 fn1 code1 true_label false_label) in
    (ti2, table4, fn2, true_label, false_label)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid condition expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

condition_code :: Table -> String -> String -> String -> String -> String -> (Table, String)
condition_code table condition_op_code fn condition_left true_label false_label
 | condition_op_code == "||" = (table, fn ++ "if(" ++ condition_left ++ ") goto " ++ true_label ++ ";\n")
 | condition_op_code == "&&" = 
   let (table1, sc_label) = (getConditionalLabel table) in
   (table1, fn ++ "if(" ++ condition_left ++ ") goto " ++ sc_label ++ ";\n" ++
        "goto " ++ false_label ++ ";\n" ++ sc_label ++ ":;\n")
 
-- condition_expression_tail -> ε | condition_op condition
condition_expression_tail :: TokenIterator -> Table -> String -> String -> String -> String -> (TokenIterator, Table, String)
condition_expression_tail (currentToken, rest, cline) table fn condition_left true_label false_label
  | (tokenType currentToken) `elem` (predict "condition_op") =
    let (ti1, code1) = (condition_op (currentToken, rest, cline)) in 
    let (table1, fn1) = (condition_code table code1 fn condition_left true_label false_label) in
    let (ti2, code2, table2, fn2) = (condition ti1 table1 fn1) in
    (ti2, table2, fn2 ++ "if(" ++ code2 ++ ")" ++ " goto " ++ true_label ++ ";\n" ++
    "goto " ++ false_label ++ ";\n")
  | (tokenType currentToken) `elem` (parseTable "condition_expression_tail" "FOLLOW") =
    ((currentToken, rest, cline), table, fn ++ 
    "if(" ++ condition_left ++ ") goto "  ++ true_label ++ ";\n" ++ 
    "goto " ++ false_label ++ ";\n")
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid condition expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- condition_op -> && | ||
condition_op :: TokenIterator -> (TokenIterator, String)
condition_op (currentToken, rest, cline)
  | (tokenType currentToken) == "&&" =
    let ti1 = (match (currentToken, rest, cline) "&&") in
    (ti1, "&&")
  | (tokenType currentToken) == "||" =
    let ti1 = (match (currentToken, rest, cline) "||") in
    (ti1, "||")
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid condition operator at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- condition -> expression comparison_op expression
condition :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
condition (currentToken, rest, cline) table fn
  | (tokenType currentToken) `elem` (predict "expression") =
    let (ti1, code1, table1, fn1) = (expression (currentToken, rest, cline) table fn) in
    let (ti2, code2) = (comparison_op ti1) in 
    let (ti3, code3, table2, fn2) = (expression ti2 table1 fn1) in
    (ti3, code1 ++ code2 ++ code3, table2, fn2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid condition expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
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
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid comparison operator at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- while_statement -> while ( condition_expression ) block_statements	
while_statement :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
while_statement (currentToken, rest, cline) table fn
  | (tokenType currentToken) == "while" =
    let ti1 = (match (currentToken, rest, cline) "while") in
    let ti2 = (match ti1 "(") in
    let (table1, begin_label) = (getConditionalLabel table) in
    let fn1 = (fn ++ begin_label ++ ":;\n") in
    let (ti3, table2, fn2, true_label, false_label) = (condition_expression ti2 table1 fn1) in
    let ti4 = (match ti3 ")") in
    let (ti5, code1, table3) = (block_statements ti4 table2 [begin_label, false_label]) in
    (ti5, true_label ++ ":;\n" ++ code1 ++ "goto " ++ begin_label ++ ";\n" ++ false_label ++ ":;\n" , table3, fn2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid while statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- return_statement -> return return_statement_tail
return_statement :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
return_statement (currentToken, rest, cline) table fn
  | (tokenType currentToken) == "return" =
    let ti1 = (match (currentToken, rest, cline) "return") in
    let (ti2, code1, table1, fn1) = (return_statement_tail ti1 table fn) in
    (ti2, code1, table1, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid return statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- return_statement_tail -> expression ; | ;
return_statement_tail :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
return_statement_tail (currentToken, rest, cline) table fn
  | (tokenType currentToken) `elem` (predict "expression") =
    let (ti1, code1, table1, fn1) = (expression (currentToken, rest, cline) table fn) in
    let ti2 = (match ti1 ";") in
    (ti2, epilogue (Just code1), table1, fn1)
  | (tokenType currentToken) == ";" =
    let ti1 = (match (currentToken, rest, cline) ";") in
    (ti1, epilogue Nothing, table, fn)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid return statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- break_statement -> break ;
break_statement :: TokenIterator -> Table -> [String] -> (TokenIterator, String, Table)
break_statement (currentToken, rest, cline) table label
  | currentToken == "break" =
    let ti1 = (match (currentToken, rest, cline) "break") in
    let ti2 = (match ti1 ";") in
    let code1 = (if null label then "\n" else ("goto " ++ (label !! 1) ++ ";\n")) in
    (ti2, code1, table)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid break statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- continue_statement -> continue ;
continue_statement :: TokenIterator -> Table -> [String] -> (TokenIterator, String, Table)
continue_statement (currentToken, rest, cline) table label
  | currentToken == "continue" =
    let ti1 = (match (currentToken, rest, cline) "continue") in
    let ti2 = (match ti1 ";") in
    let code1 = (if null label then "\n" else ("goto " ++ (label !! 0) ++ ";\n")) in
    (ti2, code1, table)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid continue statement at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- expression -> term expression_tail
expression :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
expression (currentToken, rest, cline) table fn
  | (tokenType currentToken) `elem` (predict "term") =
    let (ti1, code1, table1, fn1) = (term (currentToken, rest, cline) table fn) in
    let (ti2, code2, table2, fn2) = (expression_tail ti1 table1 fn1 code1) in
    (ti2, code2, table2, fn2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- expression_tail -> ε | addop term expression_tail
expression_tail :: TokenIterator -> Table -> String -> String -> (TokenIterator, String, Table, String)
expression_tail (currentToken, rest, cline) table fn et_left
  | (tokenType currentToken) `elem` (predict "addop") =
    let (ti1, code1) = (addop (currentToken, rest, cline)) in
    let (ti2, code2, table1, fn1) = (term ti1 table fn) in
    let (table2, et_place) = (addTemp table1) in
    let fn2 = (fn1 ++ et_place ++ " = " ++ et_left ++ code1 ++ code2 ++ ";\n") in
    let (ti3, code3, table3, fn3) = (expression_tail ti2 table2 fn2 et_place) in
    (ti3, code3, table3, fn3)
  | (tokenType currentToken) `elem` (parseTable "expression_tail" "FOLLOW") =
    ((currentToken, rest, cline), et_left, table, fn)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")

-- addop -> + | -
addop :: TokenIterator -> (TokenIterator, String)
addop (currentToken, rest, cline)
  | (tokenType currentToken) == "+" =
    let ti1 = (match (currentToken, rest, cline) "+") in
    (ti1, " + ")
  | (tokenType currentToken) == "-" =
    let ti1 = (match (currentToken, rest, cline) "-") in
    (ti1, " - ")
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid add operator at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- term -> factor term_tail
term :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
term (currentToken, rest, cline) table fn
  | (tokenType currentToken) `elem` (predict "factor") =
    let (ti1, code1, table1, fn1) = (factor (currentToken, rest, cline) table fn) in
    let (ti2, code2, table2, fn2) = (term_tail ti1 table1 fn1 code1) in
    (ti2, code2, table2, fn2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expresion at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- term_tail -> ε | mulop factor term_tail
term_tail :: TokenIterator -> Table -> String -> String -> (TokenIterator, String, Table, String)
term_tail (currentToken, rest, cline) table fn tt_left
  | (tokenType currentToken) `elem` (predict "mulop") =
    let (ti1, code1) = (mulop (currentToken, rest, cline)) in
    let (ti2, code2, table1, fn1) = (factor ti1 table fn) in
    let (table2, tt_place) = (addTemp table1) in
    let fn2 = (fn1 ++ tt_place ++ " = " ++ tt_left ++ code1 ++ code2 ++ ";\n") in
    let (ti3, code3, table3, fn3) = (term_tail ti2 table2 fn2 tt_place) in
    (ti3, code3, table3, fn3)
  | (tokenType currentToken) `elem` (parseTable "term_tail" "FOLLOW") =
    ((currentToken, rest, cline), tt_left, table, fn)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- mulop -> * | /
mulop :: TokenIterator -> (TokenIterator, String)
mulop (currentToken, rest, cline)
  | (tokenType currentToken) == "*" =
    let ti1 = (match (currentToken, rest, cline) "*") in
    (ti1, " * ")
  | (tokenType currentToken) == "/" =
    let ti1 = (match (currentToken, rest, cline) "/") in
    (ti1, " / ")
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid multiply operator at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- factor -> identifier factor_tail | number | - number | ( expression )  
factor :: TokenIterator -> Table -> String -> (TokenIterator, String, Table, String)
factor (currentToken, rest, cline) table fn
  | (tokenType currentToken) == "identifier" =
    let ti1 = (match (currentToken, rest, cline) "identifier") in
    let (ti2, code1, table1, fn1) = (factor_tail ti1 table fn (extractValue currentToken)) in
    (ti2, code1, table1, fn1)
  | (tokenType currentToken) == "number" =
    let ti1 = (match (currentToken, rest, cline) "number") in
    let (table1, named_var) = (addTemp table) in
    let fn1 = (fn ++ named_var ++ " = " ++ (extractValue currentToken) ++ ";\n") in
    (ti1, named_var, table1, fn1)
  | (tokenType currentToken) == "-" =
    let ti1 = (match (currentToken, rest, cline) "-") in
    let ti2 = (match ti1 "number") in
    let (table1, named_var) = (addTemp table) in
    let fn1 = (fn ++ named_var ++ " = " ++ "-" ++ (extractValue (tiHead ti1)) ++ ";\n") in
    (ti2, named_var, table1, fn1)
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (ti2, code1, table1, fn1) = (expression ti1 table fn) in
    let ti3 = (match ti2 ")") in
    (ti3, code1, table1, fn1)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- factor_tail -> ε | ( expr_list )
factor_tail :: TokenIterator -> Table -> String -> String -> (TokenIterator, String, Table, String)
factor_tail (currentToken, rest, cline) table fn idenValue
  | (tokenType currentToken) `elem` (parseTable "factor_tail" "FOLLOW") =
    let named_var = (getMemVar table idenValue) in
    ((currentToken, rest, cline), named_var, table, fn)
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (ti2, varList1, table1, fn1) = (expr_list ti1 table fn) in
    let ti3 = (match ti2 ")") in
    let (table2, named_var) = (addTemp table1) in
    let (table3, retLabel) = (getReturnLabel table2) in
    let code1 = ((preJump retLabel varList1) ++ "goto " ++ idenValue ++ "Func;\n" ++ (postJump retLabel (Just named_var))) in
    let fn2 = (fn1 ++ code1) in
    (ti3, named_var, table3, fn2)
  | otherwise = errorWithoutStackTrace ("Syntax Error: invalid expression at line " ++ (show cline) ++ ": unexpected token " ++ "\"" ++ currentToken ++ "\"")
  
-- ghc --make translator
main = do
  (inputFile:args) <- getArgs
  tokens <- scan inputFile
-- Get comments
  let comments = unlines (getComments tokens)
-- Print comments
  putStrLn comments
-- Init Symbol table
  let symbolTable = Table 1 1 (Temps (Counts 0 [], Counts 0 [], Counts 0 []))
-- Start parsing
  let (_, programCode, _) = program (nextToken tokens 1) symbolTable
-- Print generated code 
  putStrLn programCode

