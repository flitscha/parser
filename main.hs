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

exampleGrammar2 :: Grammar
exampleGrammar2 =
    [ ("S", [[NT "A", NT "B", NT "C", NT "D", NT "E"]])
    , ("A", [[T "a"], []])
    , ("B", [[T "b"], []])
    , ("C", [[T "c"]])
    , ("D", [[T "d"], []])
    , ("E", [[T "e"], []])
    ]

exampleGrammar3 :: Grammar
exampleGrammar3 =
    [ ("S", [[NT "B", T "b"], [NT "C", T "d"]])
    , ("B", [[T "a", NT "B"], []])
    , ("C", [[T "c", NT "C"], []])
    ]

exampleGrammar4 :: Grammar
exampleGrammar4 =
    [ ("S", [[T "a", NT "B", NT "D", T "h"]])
    , ("B", [[T "c", NT "C"]])
    , ("C", [[T "b", NT "C"], []])
    , ("D", [[NT "E", NT "F"]])
    , ("E", [[T "g"], []])
    , ("F", [[T "f"], []])
    ]

exampleGrammar5 :: Grammar
exampleGrammar5 =
    [ ("S", [[NT "A", NT "C", NT "B"], [NT "C", T "b", NT "B"], [NT "B", T "a"]])
    , ("A", [[T "d", T "a"], [NT "B", NT "C"]])
    , ("B", [[T "g"], []])
    , ("C", [[T "h"], []])
    ]


main :: IO ()
main = do
    let testGrammar = exampleGrammar4
    putStrLn "\nGrammar:"
    putStrLn $ showGrammar testGrammar
    putStr "all non-terminals: "
    print $ getAllNonTerminals testGrammar
    putStr "all terminals: "
    print $ getAllTerminals testGrammar
    putStrLn "\nFirst-table:"
    print $ firstTable testGrammar