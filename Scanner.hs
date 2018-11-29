module Scanner (Token(..), scan) where

import System.IO
import Data.Char

{-
Author: Jonah Shafran
11/5/2018
-}

data Token = Number String
           | String String
           | Reserved String
           | Symbol String
           | Comment String
           | Identifier String deriving (Show)


--Takes a file specified by the file path and returns a list of tokens
scan :: FilePath -> IO [Token]
scan path = do
    contents <- readFile path
    return (tokenize contents)

--Takes a string and breaks it up into a list of tokens
tokenize :: String  -> [Token]
tokenize ('i':'f':r) = (Reserved "if") : (tokenize r)
tokenize ('i':'n':'t':r) = (Reserved "int") : (tokenize r)
tokenize ('v':'o':'i':'d':r) = (Reserved "void") : (tokenize r)
tokenize ('e':'l':'s':'e':r) = (Reserved "else") : (tokenize r)
tokenize ('b':'r':'e':'a':'k':r) = (Reserved "break") : (tokenize r)
tokenize ('w':'h':'i':'l':'e':r) = (Reserved "while") : (tokenize r)
tokenize ('s':'c':'a':'n':'f':r) = (Reserved "scanf") : (tokenize r)
tokenize ('p':'r':'i':'n':'t':'f':r) = (Reserved "printf") : (tokenize r)
tokenize ('r':'e':'t':'u':'r':'n':r) = (Reserved "return") : (tokenize r)
tokenize ('c':'o':'n':'t':'i':'n':'u':'e':r) = (Reserved "continue") : (tokenize r)
tokenize ('r':'e':'a':'d':r) = (Reserved "read") : (tokenize r)
tokenize ('w':'r':'i':'t':'e':r) = (Reserved "write") : (tokenize r)
tokenize ('{':r) = (Symbol "{") : (tokenize r)
tokenize ('}':r) = (Symbol "}") : (tokenize r)
tokenize ('(':r) = (Symbol "(") : (tokenize r)
tokenize (')':r) = (Symbol ")") : (tokenize r)
tokenize (',':r) = (Symbol ",") : (tokenize r)
tokenize (';':r) = (Symbol ";") : (tokenize r)
tokenize ('+':r) = (Symbol "+") : (tokenize r)
tokenize ('-':r) = (Symbol "-") : (tokenize r)
tokenize ('*':r) = (Symbol "*") : (tokenize r)
tokenize ('/':'/':r) = scanComment "//" r
tokenize ('/':r) = (Symbol "/") : (tokenize r)
tokenize ('=':r) = scanEqual '=' r
tokenize ('<':r) = scanEqual '<' r
tokenize ('>':r) = scanEqual '>' r
tokenize ('!':r) = scanEqual '!' r
tokenize ('&':'&':r) = (Symbol "&&") : (tokenize r)
tokenize ('&':r) = (Symbol "&") : (tokenize r)
tokenize ('|':'|':r) = (Symbol "||") : (tokenize r)
tokenize ('|':r) = (Symbol "|") :(tokenize r)
tokenize ('#':r) = scanComment "#" r
tokenize ('"':r) = scanString (String "\"") r
tokenize ('\n':r) = (Symbol "\n") : (tokenize r)
tokenize ('@':r) = (Symbol "@") : (tokenize r)
tokenize ('$':r) = (Symbol "$") : (tokenize r)
tokenize ('%':r) = (Symbol "%") : (tokenize r)
tokenize ('^':r) = (Symbol "^") : (tokenize r)
tokenize ('~':r) = (Symbol "~") : (tokenize r)
tokenize ('?':r) = (Symbol "?") : (tokenize r)
tokenize ('\\':r) = (Symbol "\\") : (tokenize r)
tokenize (h:t)
 | isDigit h = scanNum (Number [h]) t
 | isLetter h = scanIdent (Identifier [h]) t
 | otherwise = tokenize t
tokenize "" = []

--Takes a number token and a string and appends the remaining digits to the token, it then calls tokenize to generate the remaining token list
scanNum :: Token -> String -> [Token]
scanNum (Number n) (h:t)
 | isDigit h = scanNum (Number (n++[h])) t
 | isLetter h = errorWithoutStackTrace "Scan Error: Malformed Number Token"
 | otherwise = (Number n) : (tokenize (h:t))

--Takes an identifier token and a string and appends the remaining characters of the identifier to the token, it then calls tokenize to generate the remaining token list
scanIdent :: Token -> String  -> [Token]
scanIdent (Identifier s) ('_':t) = scanIdent (Identifier (s++"_")) t
scanIdent (Identifier s) (h:t)
 | (||) (isDigit h) (isLetter h) = scanIdent (Identifier (s++[h])) t
 | otherwise = (Identifier s) : (tokenize (h:t))

--Takes a character and a string and generates the "equal" version as necessary (i.e. < vs. <=), it then calls tokenize to generate the remaining token list
scanEqual :: Char -> String  -> [Token]
scanEqual c ('=':r) = (Symbol (c:"=")) : (tokenize r)
scanEqual '!' s = errorWithoutStackTrace "Scan Error: Malformed Token, Expected '!=' found '!'"
scanEqual c s = (Symbol [c]) : (tokenize s)

--takes a string containing the comment processed and a string containing the file and takes the first from the file and puts it onto the last of the comment until a \n is encounterred
scanComment :: String -> String  -> [Token]
scanComment s ('\n':r) = (Comment s) : (tokenize r)
scanComment s (h:t) = scanComment (s++[h]) t

--takes a String token and a string and continues to apped to the token until a " is encounterred
scanString :: Token -> String  -> [Token]
scanString (String s) ('\"':r) = [(String (s++"\""))] ++ (tokenize r)
scanString (String s) (h:t) = scanString (String (s++[h])) t
