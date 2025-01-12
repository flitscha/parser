module ParserGenerator where

import Grammar
import Data.List (delete, (\\))
import qualified Data.Map as Map
import Data.List (intercalate)

-- we generate the parsing-table in this module


-- The function First(X) returns the set of first symbols that can be derived from a symbol X.
-- However, the grammar should not be cyclical. Otherwise the function will not terminate.
firstTable :: Grammar -> [(NonTerminal, [Terminal])]
firstTable grammar = map (\nt -> (nt, computeFirst grammar nt)) (getAllNonTerminals grammar)

computeFirst :: Grammar -> NonTerminal -> [Terminal]
computeFirst grammar nt = removeDups $ concatMap (firstOfConclusion grammar) (getConclusionsOfNT grammar nt) where

firstOfConclusion :: Grammar -> Conclusion -> [Terminal]
firstOfConclusion _ [] = [epsilon]
firstOfConclusion _ (T terminalSymbol : _) = [terminalSymbol]
firstOfConclusion grammar (NT nonTerminal : conclusions) = 
    if epsilon `elem` computeFirst grammar nonTerminal then
        delete epsilon (computeFirst grammar nonTerminal) ++ firstOfConclusion grammar conclusions
    else 
        computeFirst grammar nonTerminal


-- The Function Follow(X) Returns the set of terminals that can appear immediately after X in a derivative. 
followTable :: Grammar -> [(NonTerminal, [Terminal])]
followTable grammar = map (\nt -> (nt, computeFollow grammar nt)) (getAllNonTerminals grammar) where

computeFollow :: Grammar -> NonTerminal -> [Terminal]
computeFollow grammar nt 
    | nt == startSymbol = removeDups $ epsilon : concatMap (followInProduction grammar nt) grammar
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
                -- if epsilon is in firstOfRest, then everything can disappear in the conclusion.
                -- in this case we also have to look at possible following symbols of the lhs-nonterminal
                followFromRest = if epsilon `elem` firstOfRest && nt /= lhs
                                    then (firstOfRest \\ [epsilon]) ++ computeFollow grammar lhs
                                    else firstOfRest \\ [epsilon]
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
-- If a Epslon appears, the next symbol is also looked at.
firstOfSequence :: Grammar -> [Symbol] -> [Terminal]
firstOfSequence _ [] = [epsilon]
firstOfSequence grammar (T terminalSymbol : _) = [terminalSymbol]
firstOfSequence grammar (NT nonTerminal : rest) =
    let firstOfNT = computeFirst grammar nonTerminal
    in if epsilon `elem` firstOfNT
        then (firstOfNT \\ [epsilon]) ++ firstOfSequence grammar rest
        else firstOfNT


-- creating the parsing table using first() and follow()
type ParsingTable = Map.Map (NonTerminal, Terminal) Conclusion

createParsingTable :: Grammar -> ParsingTable
createParsingTable grammar = createParsingTableAux grammar [] []

createParsingTableAux :: Grammar -> [(NonTerminal, [Terminal])] -> [(NonTerminal, [Terminal])] -> ParsingTable
createParsingTableAux grammar firstTable followTable = foldl insertProduction Map.empty grammar where
    -- insert every Production into the table.
    insertProduction :: ParsingTable -> Production -> ParsingTable
    insertProduction parsingTable (nt, conclusions) = foldl (insertConclusion nt) parsingTable conclusions
    -- insert every Conclusion of one Production into the table (we have to remember the nonTerminal in the premise)
    insertConclusion :: NonTerminal -> ParsingTable -> Conclusion -> ParsingTable
    insertConclusion nt parsingTable conclusion = 
        let firstSet = firstOfConclusion grammar conclusion
            followSet = computeFollow grammar nt
            -- for which terminals can we insert this rule?
            terminalsToApply = if epsilon `elem` firstSet
                then (firstSet \\ [epsilon]) ++ followSet
                else firstSet
        -- insert every terminal in terminalsToApply to the table using foldl
        -- type of foldl: (a -> b -> a) -> a -> [b] -> a
        in foldl (\currentTable terminal -> Map.insert (nt, terminal) conclusion currentTable) parsingTable terminalsToApply

