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
        , testBinary
        , testUnary
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


testBinary : Test
testBinary =
    Test.describe "Built in binary functions"
        [ parseExpect parseBinaryOp "+ 1 2" (Ok <| BinaryOp ADD (int 1) (int 2))
        , parseExpect parseBinaryOp "- 1 2" (Ok <| BinaryOp SUB (int 1) (int 2))
        , parseExpect parseBinaryOp "* 1 2" (Ok <| BinaryOp MULT (int 1) (int 2))
        , parseExpect parseBinaryOp "cons 1 nil" (Ok <| BinaryOp CONS (int 1) nil)
        , parseExpect parseBinaryOp "< 1 2" (Ok <| BinaryOp (COMPARE CMP_LT) (int 1) (int 2))
        , parseExpect parseBinaryOp "> 1 2" (Ok <| BinaryOp (COMPARE CMP_GT) (int 1) (int 2))
        , parseExpect parseBinaryOp "<= 1 2" (Ok <| BinaryOp (COMPARE CMP_LEQ) (int 1) (int 2))
        , parseExpect parseBinaryOp ">= 1 2" (Ok <| BinaryOp (COMPARE CMP_GEQ) (int 1) (int 2))
        , parseExpect parseBinaryOp "eq 1 2" (Ok <| BinaryOp (COMPARE CMP_EQ) (int 1) (int 2))
        , parseExpect parseBinaryOp "* (+ 6 2) 3" (Ok <| BinaryOp MULT (BinaryOp ADD (int 6) (int 2)) (int 3))
        ]


testUnary : Test
testUnary =
    Test.describe "Build in Unary operands"
        [ parseExpect parseUnaryOp "atom 4" (Ok <| UnaryOp ATOM (int 4))
        , parseExpect parseUnaryOp "atom nil" (Ok <| UnaryOp ATOM nil)
        , parseExpect parseUnaryOp "null nil" (Ok <| UnaryOp NULL nil)
        , parseExpect parseUnaryOp "null x" (Ok <| UnaryOp NULL (var "x"))
        , parseExpect parseUnaryOp "cdr nil" (Ok <| UnaryOp CDR nil)
        , parseExpect parseUnaryOp "car nil" (Ok <| UnaryOp CAR nil)
        ]


testIf : Test
testIf =
    Test.describe "If statements"
        [ parseExpect parseIf "if (eq 1 2) 3 4" (Ok <| If (BinaryOp (COMPARE CMP_EQ) (int 1) (int 2)) (int 3) (int 4))
        , parseExpect parseIf "if (null x) y (cdr x)" (Ok <| If (UnaryOp NULL (var "x")) (var "y") (UnaryOp CDR (var "x")))
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
    Test.describe "Lambda functions"
        [ parseExpect parseLambda "lambda (x) x" (Ok <| Lambda [ token "x" ] (var "x"))
        , parseExpect parseLambda "lambda    (x)  x   " (Ok <| Lambda [ token "x" ] (var "x"))
        , parseExpect parseLambda "lambda (x y) (+ x y)" (Ok <| Lambda [ token "x", token "y" ] (BinaryOp ADD (var "x") (var "y")))
        , parseExpect parseLambda "lambda  (x y z) x " (Ok <| Lambda [ token "x", token "y", token "z" ] (var "x"))
        , parseExpect parseLambda "lambda () 3" (Ok <| Lambda [] (int 3))
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
