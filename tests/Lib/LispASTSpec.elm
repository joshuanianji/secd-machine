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
        , testLet
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
        [ binaryUnarySuccess
        , nArySuccess
        , complexApp
        ]


binaryUnarySuccess : Test
binaryUnarySuccess =
    Test.describe "Binary and Unary Functions succeed on" <|
        List.map (\( input, expected ) -> parseExpect parseFunctionApp input (Ok expected))
            [ ( "x", FuncApp (var "x") [] )
            , ( "atom 4", FuncApp (var "atom") [ int 4 ] )
            , ( "atom nil", FuncApp (var "atom") [ nil ] )
            , ( "null nil", FuncApp (var "null") [ nil ] )
            , ( "null x", FuncApp (var "null") [ var "x" ] )
            , ( "cdr nil", FuncApp (var "cdr") [ nil ] )
            , ( "car nil", FuncApp (var "car") [ nil ] )
            , ( "+ 1 2", FuncApp (var "+") [ int 1, int 2 ] )
            , ( "- 1 2", FuncApp (var "-") [ int 1, int 2 ] )
            , ( "* 1 2", FuncApp (var "*") [ int 1, int 2 ] )
            , ( "cons 1 nil", FuncApp (var "cons") [ int 1, nil ] )
            , ( "< 1 2", FuncApp (var "<") [ int 1, int 2 ] )
            , ( "> 1 2", FuncApp (var ">") [ int 1, int 2 ] )
            , ( "<= 1 2", FuncApp (var "<=") [ int 1, int 2 ] )
            , ( ">= 1 2", FuncApp (var ">=") [ int 1, int 2 ] )
            , ( "eq 1 2", FuncApp (var "eq") [ int 1, int 2 ] )
            , ( "* (+ 6 2) 3", FuncApp (var "*") [ FuncApp (var "+") [ int 6, int 2 ], int 3 ] )
            ]



-- in reality i wont allow a "+" function to take more than 2 args


nArySuccess : Test
nArySuccess =
    Test.describe "N-ary functions succeed on" <|
        List.map (\( input, expected ) -> parseExpect parseFunctionApp input (Ok expected))
            [ ( "list 1 2 3 4 5", FuncApp (var "list") [ int 1, int 2, int 3, int 4, int 5 ] )
            , ( "list 1 2 3 4 5 6 7 8 9 10", FuncApp (var "list") [ int 1, int 2, int 3, int 4, int 5, int 6, int 7, int 8, int 9, int 10 ] )
            , ( "+ 1 2 3 4 5", FuncApp (var "+") [ int 1, int 2, int 3, int 4, int 5 ] )
            ]



-- sometimes, the function we are applying arguments to can be an inline lambda function


complexApp : Test
complexApp =
    Test.describe "Complex Function Application" <|
        List.map (\( input, expected ) -> parseExpect parseFunctionApp input (Ok expected))
            [ ( "(lambda (x y) (+ x y)) 3 4"
              , FuncApp
                    (Lambda [ token "x", token "y" ] (FuncApp (var "+") [ var "x", var "y" ]))
                    [ int 3, int 4 ]
              )
            , ( "((lambda (x y) (+ x y)) 3) 4"
              , FuncApp
                    (FuncApp
                        (Lambda [ token "x", token "y" ] (FuncApp (var "+") [ var "x", var "y" ]))
                        [ int 3 ]
                    )
                    [ int 4 ]
              )
            ]


testIf : Test
testIf =
    Test.describe "If statements"
        [ parseExpect parseIf "if (eq 1 2) 3 4" (Ok <| If (FuncApp (var "eq") [ int 1, int 2 ]) (int 3) (int 4))
        , parseExpect parseIf "if (null x) y (cdr x)" (Ok <| If (FuncApp (var "null") [ var "x" ]) (var "y") (FuncApp (var "cdr") [ var "x" ]))
        , parseExpect parseIf "if  ( null   x )  y ( cdr x  ) " (Ok <| If (FuncApp (var "null") [ var "x" ]) (var "y") (FuncApp (var "cdr") [ var "x" ]))
        , parseExpect parseIf "if(null x)y(cdr x) " (Ok <| If (FuncApp (var "null") [ var "x" ]) (var "y") (FuncApp (var "cdr") [ var "x" ]))
        ]



-- we use parseTokens to parse the arguments for lambda expressions


testTokens : Test
testTokens =
    Test.describe "A list of tokens"
        [ parseExpect (parseList parseToken) "()" (Ok <| [])
        , parseExpectErr (parseList parseToken) "(1 2 3)"
        , parseExpect (parseList parseToken) "(x y    z)" (Ok <| [ token "x", token "y", token "z" ])
        , parseExpect (parseList parseToken) "(x)" (Ok <| [ token "x" ])
        , parseExpect (parseList parseToken) "(  x )" (Ok <| [ token "x" ])
        ]


testLambda : Test
testLambda =
    Test.describe "Lambda functions" <|
        List.map (\( input, expected ) -> parseExpect parseLambda input (Ok expected))
            [ ( "lambda (x) x", Lambda [ token "x" ] (var "x") )
            , ( "lambda    (x)  x   ", Lambda [ token "x" ] (var "x") )
            , ( "lambda (x y) (+ x y)", Lambda [ token "x", token "y" ] (FuncApp (var "+") [ var "x", var "y" ]) )
            , ( "lambda  (x y z) x ", Lambda [ token "x", token "y", token "z" ] (var "x") )
            , ( "lambda () 3", Lambda [] (int 3) )
            ]



-- parse let and letrec


testLet : Test
testLet =
    Test.describe "Let and Letrec bindings"
        [ testLetSuccess
        , testLetRec
        ]


testLetSuccess : Test
testLetSuccess =
    Test.describe "Let statements that should succeed" <|
        List.map
            (\( input, expected ) -> parseExpect parseLet input (Ok expected))
            [ ( "let (x y) (1 2) (+ x y)"
              , Let
                    [ ( token "x", int 1 ), ( token "y", int 2 ) ]
                    (FuncApp (var "+") [ var "x", var "y" ])
              )
            , ( "let (x y )(1 2)   (+ x y )"
              , Let
                    [ ( token "x", int 1 ), ( token "y", int 2 ) ]
                    (FuncApp (var "+") [ var "x", var "y" ])
              )
            , ( "let (x) (((lambda (y) (+ y 1)) 2)) (* x x)"
              , Let
                    [ ( token "x", FuncApp (Lambda [ token "y" ] (FuncApp (var "+") [ var "y", int 1 ])) [ int 2 ] ) ]
                    (FuncApp (var "*") [ var "x", var "x" ])
              )
            ]


testLetRec : Test
testLetRec =
    Test.describe "Recursive functions using letrec"
        [ Test.todo "Add test cases after we implement the QUOTE function" ]



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
