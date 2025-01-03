module ParserGenerator where

import Grammar
import Data.List (delete, (\\))
import qualified Data.Map as Map

-- we generate the parsing-table in this module


-- The function First(X) returns the set of first symbols that can be derived from a symbol X.
-- However, the grammar should not be cyclical. Otherwise the function will not terminate.
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


-- The Function Follow(X) Returns the set of terminals that can appear immediately after X in a derivative. 
followTable :: Grammar -> [(NonTerminal, [Terminal])]
followTable grammar = map (\nt -> (nt, computeFollow grammar nt)) (getAllNonTerminals grammar) where

computeFollow :: Grammar -> NonTerminal -> [Terminal]
computeFollow grammar nt 
    | nt == startSymbol = removeDups $ "$" : concatMap (followInProduction grammar nt) grammar
    | otherwise = removeDups $ concatMap (followInProduction grammar nt) grammar where
        startSymbol = fst (head grammar)

followInProduction :: Grammar -> NonTerminal -> Production -> [Terminal]
followInProduction grammar nt (lhs, conclusions) = concatMap (followInConclusion grammar nt lhs) conclusions

--                               follow of        premise
followInConclusion :: Grammar -> NonTerminal -> NonTerminal -> Conclusion -> [Terminal]
followInConclusion grammar nt lhs conclusion =
    case findNTInConclusion conclusion of
        Nothing -> []
        Just rest ->
            let firstOfRest = firstOfSequence grammar rest
                -- if we found the nonterminal in the conclusion, we look at the first terminals after
                -- this nonterminal.
                -- if "$" is in firstOfRest, then everything can disappear in the conclusion.
                -- in this case we also have to look at possible following symbols of the lhs-nonterminal
                followFromRest = if "$" `elem` firstOfRest && nt /= lhs
                                    then (firstOfRest \\ ["$"]) ++ computeFollow grammar lhs
                                    else firstOfRest \\ ["$"]
            in followFromRest
  where
    -- Find the first occurrence of the nonterminal in a conclusion (list of Symbols)
    -- return the rest of the symbols
    findNTInConclusion [] = Nothing
    findNTInConclusion (NT x : xs)
        | x == nt = Just xs
        | otherwise = findNTInConclusion xs
    findNTInConclusion (_ : xs) = findNTInConclusion xs

-- Calculates the possible first Terminals of a sequence of symbols
-- If a '$' appears, the next symbol is also looked at.
firstOfSequence :: Grammar -> [Symbol] -> [Terminal]
firstOfSequence _ [] = ["$"]
firstOfSequence grammar (T terminalSymbol : _) = [terminalSymbol]
firstOfSequence grammar (NT nonTerminal : rest) =
    let firstOfNT = computeFirst grammar nonTerminal
    in if "$" `elem` firstOfNT
        then (firstOfNT \\ ["$"]) ++ firstOfSequence grammar rest
        else firstOfNT

