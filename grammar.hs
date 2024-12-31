module Grammar where

-- we define a context-free grammar
type NonTerminal = String
type Terminal = String

-- a symbol is either a terminal or a non-terminal
data Symbol = T Terminal | NT NonTerminal deriving (Eq)

-- In a context-free grammar, the premise is always exactly one non-terminal symbol.
-- The conclusion, on the other hand, is arbitrary.
type Conclusion = [Symbol]

-- we group all production rules for each non-terminal.
-- Here a list of conclusions is assigned for one non-terminal
type Production = (NonTerminal, [Conclusion])
type Grammar = [Production]



-- functions to print a grammar
showSymbol :: Symbol -> String
showSymbol (T t) = t
showSymbol (NT nt) = nt

-- In a conclusion, the symbols are simply put together. And an empty list corresponds to epsilon.
-- To make it readable, all symbols should consist of only one char. 
-- Non-terminals should be uppercase. Terminals should be lowercase.
showConclusion :: Conclusion -> String
showConclusion [] = "ε"
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


getAllNonTerminals :: Grammar -> [NonTerminal]
getAllNonTerminals = undefined

getAllTerminals :: Grammar -> [Terminal]
getAllTerminals = undefined