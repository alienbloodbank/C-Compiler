module ParseTable (parseTable, predict) where

-- Check from list of eps non-terminals
isEPS :: String -> Bool
isEPS s = s `elem` ["program_data", 
  "parameter_list",
  "parameter_list_tail",
  "non_empty_parameter_list",
  "data_decls",
  "id_list_tail",
  "statements",
  "expr_list",
  "non_empty_expr_list_tail",
  "if_statement_tail",
  "condition_expression_tail",
  "expression_tail",
  "term_tail",
  "factor_tail"]

-- Get predict set from parse table
predict :: String -> [String]
predict s = (parseTable s "FIRST") ++ (if (isEPS s) then (parseTable s "FOLLOW") else [])

-- Parse table for FIRST and FOLLOW sets
parseTable :: String -> String -> [String]
parseTable "program" "FIRST" = ["void", "int", "EOF"]
parseTable "program" "FOLLOW" = ["EOF"]
parseTable "program_data" "FIRST" = ["void", "int"]
parseTable "program_data" "FOLLOW" = ["EOF"]
parseTable "global_decl" "FIRST" = ["void", "int"]
parseTable "global_decl" "FOLLOW" = ["EOF", "void", "int"]
parseTable "global_decl_tail" "FIRST" = ["(", ",", ";"]
parseTable "global_decl_tail" "FOLLOW" = ["EOF", "void", "int"]
parseTable "func_tail" "FIRST" = [";", "{"]
parseTable "func_tail" "FOLLOW" = ["EOF", "void", "int"]
parseTable "type_name" "FIRST" = ["void", "int"]
parseTable "type_name" "FOLLOW" = ["identifier"]
parseTable "parameter_list" "FIRST" = ["void", "int"]
parseTable "parameter_list" "FOLLOW" = [")"]
parseTable "parameter_list_tail" "FIRST" = ["identifier"]
parseTable "parameter_list_tail" "FOLLOW" = [")"]
parseTable "non_empty_parameter_list" "FIRST" = [","]
parseTable "non_empty_parameter_list" "FOLLOW" = [")"]
parseTable "data_decls" "FIRST" = ["void", "int"]
parseTable "data_decls" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "id_list" "FIRST" = ["identifier"]
parseTable "id_list" "FOLLOW" = [";"]
parseTable "id_list_tail" "FIRST" = [","]
parseTable "id_list_tail" "FOLLOW" = [";"]
parseTable "block_statements" "FIRST" = ["{"]
parseTable "block_statements" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue", "else"]
parseTable "statements" "FIRST" = ["identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "statements" "FOLLOW" = ["}"]
parseTable "statement" "FIRST" = ["identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "statement" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "assignment_or_general_func_call" "FIRST" = ["identifier"]
parseTable "assignment_or_general_func_call" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "assignment_or_general_func_call_tail" "FIRST" = ["=", "("]
parseTable "assignment_or_general_func_call_tail" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "printf_func_call" "FIRST" = ["printf"]
parseTable "printf_func_call" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "printf_func_call_tail" "FIRST" = [")", ","]
parseTable "printf_func_call_tail" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "scanf_func_call" "FIRST" = ["scanf"]
parseTable "scanf_func_call" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "expr_list" "FIRST" = ["identifier", "number", "-", "("]
parseTable "expr_list" "FOLLOW" = [")"]
parseTable "non_empty_expr_list" "FIRST" = ["identifier", "number", "-", "("]
parseTable "non_empty_expr_list" "FOLLOW" = [")"]
parseTable "non_empty_expr_list_tail" "FIRST" = [","]
parseTable "non_empty_expr_list_tail" "FOLLOW" = [")"]
parseTable "if_statement" "FIRST" = ["if"]
parseTable "if_statement" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "if_statement_tail" "FIRST" = ["else"]
parseTable "if_statement_tail" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "condition_expression" "FIRST" = ["identifier", "number", "-", "("]
parseTable "condition_expression" "FOLLOW" = [")"]
parseTable "condition_expression_tail" "FIRST" = ["&&", "||"]
parseTable "condition_expression_tail" "FOLLOW" = [")"]
parseTable "condition" "FIRST" = ["identifier", "number", "-", "("]
parseTable "condition" "FOLLOW" = [")", "&&", "||"]
parseTable "condition_op" "FIRST" = ["&&", "||"]
parseTable "condition_op" "FOLLOW" = [")", "&&", "||", "identifier", "number", "-", "("]
parseTable "comparison_op" "FIRST" = ["==", "!=", ">", ">=", "<", "<="]
parseTable "comparison_op" "FOLLOW" = ["identifier", "number", "-", "("]
parseTable "while_statement" "FIRST" = ["while"]
parseTable "while_statement" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "return_statement" "FIRST" = ["return"]
parseTable "return_statement" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "return_statement_tail" "FIRST" = ["identifier", "number", "-", "(", ";"]
parseTable "return_statement_tail" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "break_statement" "FIRST" = ["break"]
parseTable "break_statement" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "continue_statement" "FIRST" = ["continue"]
parseTable "continue_statement" "FOLLOW" = ["}", "identifier", "printf", "scanf", "if", "while", "return", "break", "continue"]
parseTable "expression" "FIRST" = ["identifier", "number", "-", "("]
parseTable "expression" "FOLLOW" = [")", ";", "==", "!=", ">", ">=", "<", "<=", ",", "&&", "||"]
parseTable "expression_tail" "FIRST" = ["+", "-"]
parseTable "expression_tail" "FOLLOW" = [")", ";", "==", "!=", ">", ">=", "<", "<=", ",", "&&", "||"]
parseTable "addop" "FIRST" = ["+", "-"]
parseTable "addop" "FOLLOW" = ["identifier", "number", "-", "("]
parseTable "term" "FIRST" = ["identifier", "number", "-", "("]
parseTable "term" "FOLLOW" = ["+", "-", ")", ";", "==", "!=", ">", ">=", "<", "<=", ",", "&&", "||"]
parseTable "term_tail" "FIRST" = ["*", "/"]
parseTable "term_tail" "FOLLOW" = ["+", "-", ")", ";", "==", "!=", ">", ">=", "<", "<=", ",", "&&", "||"]
parseTable "mulop" "FIRST" = ["*", "/"]
parseTable "mulop" "FOLLOW" = ["identifier", "number", "-", "("]
parseTable "factor" "FIRST" = ["identifier", "number", "-", "("]
parseTable "factor" "FOLLOW" = ["*", "/", "+", "-", ")", ";", "==", "!=", ">", ">=", "<", "<=", ",", "&&", "||"]
parseTable "factor_tail" "FIRST" = ["("]
parseTable "factor_tail" "FOLLOW" = ["*", "/", "+", "-", ")", ";", "==", "!=", ">", ">=", "<", "<=", ",", "&&", "||"]
