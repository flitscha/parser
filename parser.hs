module Parser where

import ParserGenerator
import Grammar
import qualified Data.Map as Map
import Data.List (intercalate)


data SyntaxTree =
    Node NonTerminal [SyntaxTree]
  | Leaf Terminal
  | EmptyTree
  deriving (Show, Eq)



-- function to create a parser, with a grammar as input
-- this way, the parsing table gets only calculated once. (because of sharing)

-- a parser is a function [Terminal] -> (SyntaxTree, [Terminal])
-- the output of the parser is a tuple: the resulting syntax-tree and [Terminal] is the rest of the input
-- if the output is not (_, []), the input could't be parsed. 
-- [Terminal] is part of the output, to indicate where the error happened.
createParser :: Grammar -> ([Terminal] -> (SyntaxTree, [Terminal]))
createParser grammar = \input -> parseWithStack input parsingTable [NT startSymbol, T epsilon] where
    parsingTable = createParsingTable grammar
    startSymbol = getStartSymbol grammar


-- the first element of the stack-list is the first Element consumed from the stack
--                  input          table         stack              output
parseWithStack :: [Terminal] -> ParsingTable -> [Symbol] -> (SyntaxTree, [Terminal])
parseWithStack [] _ [] = (EmptyTree, []) -- successful parsed
parseWithStack [] _ (x:xs) = (EmptyTree, []) -- not successful: Stack is not empty
parseWithStack (x:xs) _ [] = (EmptyTree, x:xs) -- not successful: Stack is empty, but there is input left
parseWithStack (x:xs) _ (T terminal : _) -- first element in the stack is a terminal
    | x /= terminal = (EmptyTree, x:xs) -- the terminal does not match with the input
    | otherwise = (Leaf terminal, xs)
-- next sybol on the stack is a nonTerminal
parseWithStack (x:xs) table (NT nonTerminal : _) =
    case Map.lookup (nonTerminal, x) table of -- transform the nonTerminal with a rule of the grammar
        Nothing -> (EmptyTree, x:xs)
        Just [] -> (EmptyTree, x:xs) -- error: there was no rule in the table
        Just rule ->
            let (subtrees, remainingInput) = parseSequence (x:xs) table rule
            in (Node nonTerminal subtrees, remainingInput)


-- auxiliary function to create one syntax tree for each symbol on the stack
-- this is used, when a rule is used. For example: S -> AaB
-- in this case a tree with root S is constructed. This tree has 3 Subtrees with the Roots A, a and B
--                  input          table         stack    output: multiple syntax trees
parseSequence :: [Terminal] -> ParsingTable -> [Symbol] -> ([SyntaxTree], [Terminal])
parseSequence input _ [] = ([], input) -- empty Stack: no trees are returned
parseSequence input table (s:symbols) = (child : children, finalInput) where
    (child, remainingInput) = parseWithStack input table [s]
    (children, finalInput) = parseSequence remainingInput table symbols



showTree :: SyntaxTree -> String
showTree tree = go 0 tree where
    -- Helper function with indentation level
    go :: Int -> SyntaxTree -> String
    go indent EmptyTree = replicate indent ' ' ++ "∅" -- Empty tree
    go indent (Leaf terminal) = replicate indent ' ' ++ show terminal
    go indent (Node nonTerminal children) =
        replicate indent ' ' ++ show nonTerminal ++ "\n" ++
        intercalate "\n" (map (go (indent + 2)) children)