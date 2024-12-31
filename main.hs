module Main where

import Grammar

-- example grammar
exampleGrammar :: Grammar
exampleGrammar =
    [ ("S", [[NT "A", T "a"], [T "b"]])
    , ("A", [[T "c", T "a"]])
    ]


main :: IO ()
main = do
    putStrLn ""
    putStrLn $ showGrammar exampleGrammar
    print $ getAllNonTerminals exampleGrammar
    print $ getAllTerminals exampleGrammar