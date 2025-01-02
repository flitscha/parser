module Main where

import Grammar
import ParserGenerator

-- example grammar
exampleGrammar :: Grammar
exampleGrammar =
    [ ("E", [[NT "T", NT "E'"]])
    , ("E'", [[T "+", NT "T", NT "E'"], []])
    , ("T", [[NT "F", NT "T'"]])
    , ("T'", [[T "*", NT "F", NT "T'"], []])
    , ("F", [[T "id"], [T "(", NT "E", T ")"]])
    ]


main :: IO ()
main = do
    putStrLn "\nGrammar:"
    putStrLn $ showGrammar exampleGrammar
    putStr "all non-terminals: "
    print $ getAllNonTerminals exampleGrammar
    putStr "all terminals: "
    print $ getAllTerminals exampleGrammar
    putStrLn "\nFirst-table:"
    print $ firstTable exampleGrammar