module ParserGenerator where

import Grammar
import Data.List (delete)

-- we generate the parsing-table in this module


-- The function First(X) returns the set of first symbols that can be derived from a symbol X.
firstTable :: Grammar -> [(NonTerminal, [Terminal])]
firstTable grammar = map (\nt -> (nt, computeFirst grammar nt)) (getAllNonTerminals grammar)

computeFirst :: Grammar -> NonTerminal -> [Terminal]
computeFirst grammar nt = removeDups $ concat $ map firstOfConclusion (getConclusionsOfNT grammar nt) where
    firstOfConclusion [] = ["$"]
    firstOfConclusion (T terminalSymbol : _) = [terminalSymbol]
    firstOfConclusion (NT nonTerminal : conclusions) = 
        if "$" `elem` computeFirst grammar nonTerminal then
            delete "$" (computeFirst grammar nonTerminal) ++ firstOfConclusion conclusions
        else 
            computeFirst grammar nonTerminal
 