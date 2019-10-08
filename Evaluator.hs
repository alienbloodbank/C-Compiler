module Evaluator (evaluateConstant) where

import Scanner
import ParseTable

{-
Evaluator.hs
Author: Soubhk Ghosh
11/19/2018
-}

-- Token consumer
match :: TokenIterator -> String -> TokenIterator
match (currentToken, rest, cline) expected
  | (tokenType currentToken) == expected = nextToken rest cline
  | otherwise = (reportError [currentToken] (show cline) expected)

-- expression -> term expression_tail
expression :: TokenIterator -> (TokenIterator, Int)
expression (currentToken, rest, cline)
  | (tokenType currentToken) `elem` (predict "term") =
    let (ti1, val1) = (term (currentToken, rest, cline)) in
    let (ti2, val2) = (expression_tail ti1 val1) in
    (ti2, val2)
  | otherwise = (reportError (predict "expression") (show cline) currentToken)

-- expression_tail -> ε | addop term expression_tail
expression_tail :: TokenIterator -> Int -> (TokenIterator, Int)
expression_tail (currentToken, rest, cline) et_left
  | (tokenType currentToken) `elem` (predict "addop") =
    let (ti1, op) = (addop (currentToken, rest, cline)) in
    let (ti2, val1) = (term ti1) in
    let (ti3, val2) = (expression_tail ti2 (op et_left val1)) in
    (ti3, val2)
  | (tokenType currentToken) `elem` (parseTable "expression_tail" "FOLLOW") =
    ((currentToken, rest, cline), et_left)
  | otherwise = (reportError (predict "expression_tail") (show cline) currentToken)

-- addop -> + | -
addop :: TokenIterator -> (TokenIterator, Int -> Int -> Int)
addop (currentToken, rest, cline)
  | (tokenType currentToken) == "+" =
    let ti1 = (match (currentToken, rest, cline) "+") in
    (ti1, (+))
  | (tokenType currentToken) == "-" =
    let ti1 = (match (currentToken, rest, cline) "-") in
    (ti1, (-))
  | otherwise = (reportError (predict "addop") (show cline) currentToken)
  
-- term -> factor term_tail
term :: TokenIterator -> (TokenIterator, Int)
term (currentToken, rest, cline)
  | (tokenType currentToken) `elem` (predict "factor") =
    let (ti1, val1) = (factor (currentToken, rest, cline)) in
    let (ti2, val2) = (term_tail ti1 val1) in
    (ti2, val2)
  | otherwise = (reportError (predict "term") (show cline) currentToken)
  
-- term_tail -> ε | mulop factor term_tail
term_tail :: TokenIterator -> Int -> (TokenIterator, Int)
term_tail (currentToken, rest, cline) tt_left
  | (tokenType currentToken) `elem` (predict "mulop") =
    let (ti1, op) = (mulop (currentToken, rest, cline)) in
    let (ti2, val1) = (factor ti1) in
    let (ti3, val2) = (term_tail ti2 (op tt_left val1)) in
    (ti3, val2)
  | (tokenType currentToken) `elem` (parseTable "term_tail" "FOLLOW") =
    ((currentToken, rest, cline), tt_left)
  | otherwise = (reportError (predict "term_tail") (show cline) currentToken)
  
-- mulop -> * | /
mulop :: TokenIterator -> (TokenIterator, Int -> Int -> Int)
mulop (currentToken, rest, cline)
  | (tokenType currentToken) == "*" =
    let ti1 = (match (currentToken, rest, cline) "*") in
    (ti1, (*))
  | (tokenType currentToken) == "/" =
    let ti1 = (match (currentToken, rest, cline) "/") in
    (ti1, quot)
  | otherwise = (reportError (predict "mulop") (show cline) currentToken)

-- factor -> identifier factor_tail | number | - number | ( expression )  
factor :: TokenIterator -> (TokenIterator, Int)
factor (currentToken, rest, cline)
  | (tokenType currentToken) == "number" =
    let ti1 = (match (currentToken, rest, cline) "number") in
    (ti1, read (extractValue currentToken) :: Int)
  | (tokenType currentToken) == "-" =
    let ti1 = (match (currentToken, rest, cline) "-") in
    let ti2 = (match ti1 "number") in
    (ti2, -(read (extractValue currentToken) :: Int))
  | (tokenType currentToken) == "(" =
    let ti1 = (match (currentToken, rest, cline) "(") in
    let (ti2, val1) = (expression ti1) in
    let ti3 = (match ti2 ")") in
    (ti3, val1)
  | otherwise = (reportError (predict "factor") (show cline) currentToken)

evaluateConstant = expression

