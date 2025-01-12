module Parser where

import ParserGenerator
import Grammar


data SyntaxTree =
    Node NonTerminal [SyntaxTree]
  | Leaf Terminal
  | EmptyTree
  deriving (Show, Eq)


-- the output is a tuple: the resulting syntax-tree and [Terminal] is the rest of the input
-- if the output is not (_, []), the input could't be parsed. 
-- [Terminal] is part of the output, to indicate where the error happened.
parse :: [Terminal] -> Grammar -> (SyntaxTree, [Terminal])
parse input grammar = parseWithStack input parsingTable [T epsilon, NT startSymbol] where
    parsingTable = createParsingTable grammar
    startSymbol = getStartSymbol grammar


-- the first element of the stack-list is the first Element consumed from the stack
--                  input          table         stack              output
parseWithStack :: [Terminal] -> ParsingTable -> [Symbol] -> (SyntaxTree, [Terminal])
parseWithStack [] _ [] = (EmptyTree, []) -- successful parsed
parseWithStack [] _ (x:xs) = (EmptyTree, []) -- not successful: Stack is not empty 
parseWithStack (x:xs) _ (T terminal : _) -- first element in the stack is a terminal
    | x /= terminal = (EmptyTree, x:xs) -- the terminal does not match with the input
    | otherwise = (Leaf terminal, xs)
