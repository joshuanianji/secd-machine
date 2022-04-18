module Lib.LispASTSpec exposing (suite)

import Expect
import Fuzz
import Lib.LispAST exposing (..)
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
        ]


testBasics : Test
testBasics =
    Test.describe "Basics"
        [ Test.fuzz Fuzz.int "Integers" <|
            \x ->
                Expect.equal (parse (String.fromInt x)) (Ok <| Integer x)
        , Test.test "Variable" <|
            \_ ->
                Expect.equal (parse "x") (Ok <| var "x")
        ]


testBinary : Test
testBinary =
    Test.describe "Built in binary functions"
        [ Test.test "(+ 1 2)" <|
            \_ ->
                Expect.equal (parse "(+ 1 2)") (Ok <| BinaryOp ADD (Integer 1) (Integer 2))
        , Test.test "(- 1 2)" <|
            \_ ->
                Expect.equal (parse "(- 1 2)") (Ok <| BinaryOp SUB (Integer 1) (Integer 2))
        , Test.test "(* 1 2)" <|
            \_ ->
                Expect.equal (parse "(* 1 2)") (Ok <| BinaryOp MULT (Integer 1) (Integer 2))
        , Test.test "(cons 1 nil)" <|
            \_ ->
                Expect.equal (parse "(cons 1 nil)") (Ok <| BinaryOp CONS (Integer 1) nil)
        , Test.test "(< 1 2)" <|
            \_ ->
                Expect.equal (parse "(< 1 2)") (Ok <| BinaryOp (COMPARE CMP_LT) (Integer 1) (Integer 2))
        , Test.test "(> 1 2)" <|
            \_ ->
                Expect.equal (parse "(> 1 2)") (Ok <| BinaryOp (COMPARE CMP_GT) (Integer 1) (Integer 2))
        , Test.test "(eq 1 2)" <|
            \_ ->
                Expect.equal (parse "(eq 1 2)") (Ok <| BinaryOp (COMPARE CMP_EQ) (Integer 1) (Integer 2))
        , Test.test "(<= 1 2)" <|
            \_ ->
                Expect.equal (parse "(<= 1 2)") (Ok <| BinaryOp (COMPARE CMP_LEQ) (Integer 1) (Integer 2))
        , Test.test "(>= 1 2)" <|
            \_ ->
                Expect.equal (parse "(>= 1 2)") (Ok <| BinaryOp (COMPARE CMP_GEQ) (Integer 1) (Integer 2))
        ]
