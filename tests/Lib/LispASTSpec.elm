module Lib.LispASTSpec exposing (suite)

import Expect
import Fuzz
import Lib.LispAST exposing (..)
import Parser exposing (Parser)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "LispAST Spec"
        [ parseExpectr ]


parseExpectr : Test
parseExpectr =
    Test.describe "LispAST.parser"
        [ testBasics
        , testFunctionApp
        , testIf
        , testTokens
        , testLambda
        ]


testBasics : Test
testBasics =
    Test.describe "Basics"
        [ Test.fuzz Fuzz.int "ints" <|
            \x ->
                Expect.equal (Parser.run parseInt (String.fromInt x)) (Ok <| int x)
        , Test.test "Variable" <|
            \_ ->
                Expect.equal (Parser.run parseVar "x") (Ok <| var "x")
        ]


testFunctionApp : Test
testFunctionApp =
    Test.describe "Function application"
        [ functionAppSuccess
        ]


functionAppSuccess : Test
functionAppSuccess =
    Test.describe "Succeeds on" <|
        List.map (\( input, expected ) -> parseExpect parseFunctionApp input (Ok expected))
            [ ( "x", FuncApp (token "x") [] )
            , ( "atom 4", FuncApp (token "atom") [ int 4 ] )
            , ( "atom nil", FuncApp (token "atom") [ nil ] )
            , ( "null nil", FuncApp (token "null") [ nil ] )
            , ( "null x", FuncApp (token "null") [ var "x" ] )
            , ( "cdr nil", FuncApp (token "cdr") [ nil ] )
            , ( "car nil", FuncApp (token "car") [ nil ] )
            , ( "+ 1 2", FuncApp (token "+") [ int 1, int 2 ] )
            , ( "- 1 2", FuncApp (token "-") [ int 1, int 2 ] )
            , ( "* 1 2", FuncApp (token "*") [ int 1, int 2 ] )
            , ( "cons 1 nil", FuncApp (token "cons") [ int 1, nil ] )
            , ( "< 1 2", FuncApp (token "<") [ int 1, int 2 ] )
            , ( "> 1 2", FuncApp (token ">") [ int 1, int 2 ] )
            , ( "<= 1 2", FuncApp (token "<=") [ int 1, int 2 ] )
            , ( ">= 1 2", FuncApp (token ">=") [ int 1, int 2 ] )
            , ( "eq 1 2", FuncApp (token "eq") [ int 1, int 2 ] )
            , ( "* (+ 6 2) 3", FuncApp (token "*") [ FuncApp (token "+") [ int 6, int 2 ], int 3 ] )
            ]


testIf : Test
testIf =
    Test.describe "If statements"
        [ parseExpect parseIf "if (eq 1 2) 3 4" (Ok <| If (FuncApp (token "eq") [ int 1, int 2 ]) (int 3) (int 4))
        , parseExpect parseIf "if (null x) y (cdr x)" (Ok <| If (FuncApp (token "null") [ var "x" ]) (var "y") (FuncApp (token "cdr") [ var "x" ]))
        ]



-- we use parseTokens to parse the arguments for lambda expressions


testTokens : Test
testTokens =
    Test.describe "A list of tokens"
        [ parseExpect parseTokens "()" (Ok <| [])
        , parseExpectErr parseTokens "(1 2 3)"
        , parseExpect parseTokens "(x y    z)" (Ok <| [ token "x", token "y", token "z" ])
        , parseExpect parseTokens "(x)" (Ok <| [ token "x" ])
        ]


testLambda : Test
testLambda =
    Test.describe "Lambda functions" <|
        List.map (\( input, expected ) -> parseExpect parseLambda input (Ok expected))
            [ ( "lambda (x) x"
              , Lambda [ token "x" ] (var "x")
              )
            , ( "lambda    (x)  x   "
              , Lambda [ token "x" ] (var "x")
              )
            , ( "lambda (x y) (+ x y)"
              , Lambda [ token "x", token "y" ] (FuncApp (token "+") [ var "x", var "y" ])
              )
            , ( "lambda  (x y z) x "
              , Lambda [ token "x", token "y", token "z" ] (var "x")
              )
            , ( "lambda () 3"
              , Lambda [] (int 3)
              )
            ]



-- runs a string a tests it against the expected result


parseExpect : Parser data -> String -> Result (List Parser.DeadEnd) data -> Test
parseExpect parser str res =
    Test.test str <|
        \_ ->
            Expect.equal (Parser.run parser str) res


parseExpectErr : Parser data -> String -> Test
parseExpectErr parser str =
    Test.test str <|
        \_ ->
            Expect.err (Parser.run parser str)
