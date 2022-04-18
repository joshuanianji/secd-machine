module Lib.LispASTSpec exposing (suite)

import Expect
import Fuzz
import Lib.LispAST exposing (..)
import Parser
import Test exposing (Test)


suite : Test
suite =
    Test.describe "LispAST Spec"
        [ testParser ]


testParser : Test
testParser =
    Test.describe "LispAST.parser"
        [ testBasics
        , testBinary
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
        [ Test.test "(+ 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(+ 1 2)") (Ok <| BinaryOp ADD (int 1) (int 2))
        , Test.test "(- 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(- 1 2)") (Ok <| BinaryOp SUB (int 1) (int 2))
        , Test.test "(* 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(* 1 2)") (Ok <| BinaryOp MULT (int 1) (int 2))
        , Test.test "(cons 1 nil)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(cons 1 nil)") (Ok <| BinaryOp CONS (int 1) nil)
        , Test.test "(< 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(< 1 2)") (Ok <| BinaryOp (COMPARE CMP_LT) (int 1) (int 2))
        , Test.test "(> 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(> 1 2)") (Ok <| BinaryOp (COMPARE CMP_GT) (int 1) (int 2))
        , Test.test "(eq 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(eq 1 2)") (Ok <| BinaryOp (COMPARE CMP_EQ) (int 1) (int 2))
        , Test.test "(<= 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(<= 1 2)") (Ok <| BinaryOp (COMPARE CMP_LEQ) (int 1) (int 2))
        , Test.test "(>= 1 2)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(>= 1 2)") (Ok <| BinaryOp (COMPARE CMP_GEQ) (int 1) (int 2))
        , Test.test "(* (+ 6 2) 3)" <|
            \_ ->
                Expect.equal (Parser.run parseBinaryOp "(* (+ 6 2) 3)") (Ok <| BinaryOp MULT (BinaryOp ADD (int 6) (int 2)) (int 3))
        ]


testTokens : Test
testTokens =
    Test.describe "A list of tokens"
        [ Test.test "()" <|
            \_ ->
                Expect.equal (Parser.run parseTokens "()") (Ok <| [])
        , Test.test "(x y)" <|
            \_ ->
                Expect.equal (Parser.run parseTokens "(x y)") (Ok <| [ token "x", token "y" ])
        , Test.test "(x y z)" <|
            \_ ->
                Expect.equal (Parser.run parseTokens "(x y z)") (Ok <| [ token "x", token "y", token "z" ])
        , Test.test "( x y   z )" <|
            \_ ->
                Expect.equal (Parser.run parseTokens "( x y   z )") (Ok <| [ token "x", token "y", token "z" ])
        ]


testLambda : Test
testLambda =
    Test.describe "Lambda functions"
        [ Test.test "(lambda (x) x)" <|
            \_ ->
                Expect.equal (Parser.run parseLambda "(lambda (x) x)") (Ok <| Lambda [ token "x" ] (var "x"))
        , Test.test "(  lambda    (x)  x   )" <|
            \_ ->
                Expect.equal (Parser.run parseLambda "(  lambda    (x)  x   )") (Ok <| Lambda [ token "x" ] (var "x"))
        , Test.test "(lambda (x y) (+ x y))" <|
            \_ ->
                Expect.equal (Parser.run parseLambda "(lambda (x y) (+ x y))") (Ok <| Lambda [ token "x", token "y" ] (BinaryOp ADD (var "x") (var "y")))
        , Test.test "(lambda  (x y z) x )" <|
            \_ ->
                Expect.equal (Parser.run parseLambda "(lambda  (x y z) x )") (Ok <| Lambda [ token "x", token "y", token "z" ] (var "x"))
        , Test.test "(lambda () 3)" <|
            \_ ->
                Expect.equal (Parser.run parseLambda "(lambda () 3)") (Ok <| Lambda [] (int 3))
        ]
