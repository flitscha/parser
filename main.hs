module Main where

import Grammar

-- example grammar
exampleGrammar :: Grammar
exampleGrammar =
    [ ("S", [[NT "A", T "a"], [T "b"]])
    , ("A", [[T "c"]])
    ]


main :: IO ()
main = do
    putStrLn ""
    putStrLn $ showGrammar exampleGrammar