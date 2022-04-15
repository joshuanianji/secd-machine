module SECD.VMSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import SECD.Program as Prog exposing (Func(..), Op(..), Program)
import SECD.VM as VM exposing (Value)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Test VM"
        [ testBasics
        , testBuiltIns
        , testIfElse
        ]


testBasics : Test
testBasics =
    Test.describe "Test Basic Operations"
        [ Test.test "Returns Nil in a Nil program" <|
            \_ ->
                let
                    program =
                        Prog.fromSingleton NIL
                in
                vmExpectSuccess program VM.Nil
        , Test.test "Returns Constant in a LDC program" <|
            \_ ->
                let
                    program =
                        Prog.fromSingleton (LDC 5)
                in
                vmExpectSuccess program (VM.Integer 5)
        ]



-- build in operations such as addition


testBuiltIns : Test
testBuiltIns =
    Test.describe "Tests built in operations, especially numerical ops"
        [ Test.fuzz fuzzIntPair "Adds two numbers correctly" <|
            \( a, b ) ->
                let
                    program =
                        Prog.fromList [ LDC a, LDC b, Func Add ]
                in
                vmExpectSuccess program (VM.Integer (a + b))
        , Test.fuzz fuzzIntPair "Multiplies two numbers correctly" <|
            \( a, b ) ->
                let
                    program =
                        Prog.fromList [ LDC a, LDC b, Func Mult ]
                in
                vmExpectSuccess program (VM.Integer (a * b))
        , Test.test "Addition fails when types are wrong" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, Func Add ]
                in
                vmExpectFailure program
        , Test.test "Atom function correctly identifies a number as an atom" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, Func Atom ]
                in
                vmExpectSuccess program (VM.Boolean True)
        , Test.test "Atom function fails on empty stack" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ Func Atom ]
                in
                vmExpectFailure program
        ]



-- if/else control flow


testIfElse : Test
testIfElse =
    Test.describe "Tests if/else control flow"
        [ Test.test "Correctly chooses the left side" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, Func Atom, SEL, Nested [ LDC 1, JOIN ], Nested [ LDC 2, JOIN ] ]
                in
                vmExpectSuccess program (VM.Integer 1)
        ]



-- runs the VM on the program, and expect the result to succeed and be the expected value


vmExpectSuccess : Program -> Value -> Expectation
vmExpectSuccess prog expected =
    Expect.equal (VM.evaluate <| VM.init prog) (Ok expected)


vmExpectFailure : Program -> Expectation
vmExpectFailure prog =
    Expect.err (VM.evaluate <| VM.init prog)


fuzzIntPair : Fuzzer ( Int, Int )
fuzzIntPair =
    Fuzz.tuple ( Fuzz.int, Fuzz.int )
