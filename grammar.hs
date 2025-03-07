module Grammar where

-- we define a context-free grammar
type NonTerminal = String
type Terminal = String

-- epsilon is a special case of a Terminal
epsilon :: Terminal
epsilon = "$"

-- a symbol is either a terminal or a non-terminal
data Symbol = T Terminal | NT NonTerminal deriving (Eq, Show)

-- In a context-free grammar, the premise is always exactly one non-terminal symbol.
-- The conclusion, on the other hand, is arbitrary.
type Conclusion = [Symbol]

-- we group all production rules for each non-terminal.
-- Here a list of conclusions is assigned for one non-terminal
type Production = (NonTerminal, [Conclusion])
type Grammar = [Production]


-- function to bring a grammar into its standard form. That is, all premises are distinct
simplifyGrammar :: Grammar -> Grammar
simplifyGrammar = undefined

-- functions to print a grammar
showSymbol :: Symbol -> String
showSymbol (T t) = t
showSymbol (NT nt) = nt

-- In a conclusion, the symbols are simply put together. And an empty list corresponds to epsilon.
-- To make it readable, all symbols should consist of only one char. 
-- Non-terminals should be uppercase. Terminals should be lowercase.
showConclusion :: Conclusion -> String
showConclusion (s : []) = showSymbol s
showConclusion (s : symbols) = (showSymbol s) ++ showConclusion symbols

-- To show multiple conclusions from the same premise, we separate them with " | "
showConclusionList :: [Conclusion] -> String
showConclusionList [] = ""
showConclusionList (c : []) = showConclusion c
showConclusionList (c : conclusions) = (showConclusion c) ++ " | " ++ showConclusionList conclusions


showProduction :: Production -> String
showProduction (nonTerminal, conclusions) = nonTerminal ++ " -> " ++ showConclusionList conclusions


showGrammar :: Grammar -> String
showGrammar [] = ""
showGrammar (p : productions) = (showProduction p) ++ "\n" ++ showGrammar productions


-- function to remove duplicates from a list
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (a : as) = [a] ++ removeDups (removeOne a as) where
    removeOne a [] = []
    removeOne a (x : xs)
        | a == x = removeOne a xs
        | otherwise = (x : removeOne a xs)


getStartSymbol :: Grammar -> NonTerminal
getStartSymbol ((nt, _) : _) = nt

-- get all non-terminals of a given Grammar
-- in a valid grammar, all premises should be distinct. So we don't use the removeDups-function
getAllNonTerminals :: Grammar -> [NonTerminal]
getAllNonTerminals [] = []
getAllNonTerminals ((nonTerminal, _) : productions) = [nonTerminal] ++ (getAllNonTerminals productions)


-- get all terminals of a given grammar
getAllTerminalsOfConclusion :: Conclusion -> [Terminal]
getAllTerminalsOfConclusion [] = []
getAllTerminalsOfConclusion (T terminal : symbols) = [terminal] ++ (getAllTerminalsOfConclusion symbols)
getAllTerminalsOfConclusion (NT _ : symbols) = getAllTerminalsOfConclusion symbols


getAllTerminals :: Grammar -> [Terminal]
getAllTerminals productions = removeDups (concatMap getAllTerminalsOfProduction productions) where
    getAllTerminalsOfProduction (_, conclusions) = concatMap getAllTerminalsOfConclusion conclusions 


-- get the conclusions with a given Non-Terminal as premise
getConclusionsOfNT :: Grammar -> NonTerminal -> [Conclusion]
getConclusionsOfNT [] nt = error $ "NonTerminal " ++ nt ++ " is not a premise of the grammar!"
getConclusionsOfNT (p : productions) nt
    | fst p == nt = snd p
    | otherwise = getConclusionsOfNT productions nt

-- convert a string into a list of Terminals, where each char is a Terminal
stringToTerminalList :: String -> [Terminal]
stringToTerminalList string = map (\char -> [char]) string