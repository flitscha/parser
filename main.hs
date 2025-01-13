module Main where

import Grammar
import ParserGenerator
import Parser

-- example grammar
exampleGrammar1 :: Grammar
exampleGrammar1 =
    [ ("E", [[NT "T", NT "E'"]])
    , ("E'", [[T "+", NT "T", NT "E'"], [T "$"]])
    , ("T", [[NT "F", NT "T'"]])
    , ("T'", [[T "*", NT "F", NT "T'"], [T "$"]])
    , ("F", [[T "id"], [T "(", NT "E", T ")"]])
    ]

exampleGrammar2 :: Grammar
exampleGrammar2 =
    [ ("S", [[NT "A", NT "B", NT "C", NT "D", NT "E"]])
    , ("A", [[T "a"], [T "$"]])
    , ("B", [[T "b"], [T "$"]])
    , ("C", [[T "c"]])
    , ("D", [[T "d"], [T "$"]])
    , ("E", [[T "e"], [T "$"]])
    ]

exampleGrammar3 :: Grammar
exampleGrammar3 =
    [ ("S", [[NT "B", T "b"], [NT "C", T "d"]])
    , ("B", [[T "a", NT "B"], [T "$"]])
    , ("C", [[T "c", NT "C"], [T "$"]])
    ]

exampleGrammar4 :: Grammar
exampleGrammar4 =
    [ ("S", [[T "a", NT "B", NT "D", T "h"]])
    , ("B", [[T "c", NT "C"]])
    , ("C", [[T "b", NT "C"], [T "$"]])
    , ("D", [[NT "E", NT "F"]])
    , ("E", [[T "g"], [T "$"]])
    , ("F", [[T "f"], [T "$"]])
    ]

exampleGrammar5 :: Grammar
exampleGrammar5 =
    [ ("S", [[NT "A", NT "C", NT "B"], [NT "C", T "b", NT "B"], [NT "B", T "a"]])
    , ("A", [[T "d", T "a"], [NT "B", NT "C"]])
    , ("B", [[T "g"], [T "$"]])
    , ("C", [[T "h"], [T "$"]])
    ]


main :: IO ()
main = do
    let testGrammar = exampleGrammar4
    putStrLn "\nGrammar:"
    putStrLn $ showGrammar testGrammar
    let myParser = createParser testGrammar
    let sequence = stringToTerminalList "acbgh"
    let (tree, rest) = myParser sequence
    putStr "\nSequence: "
    print sequence
    putStrLn "\nSyntax Tree:"
    putStrLn $ showTree tree
    putStr "rest: "
    print rest