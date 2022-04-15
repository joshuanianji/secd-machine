module SECD.VMSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Lib.Cons as Cons
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
                vmExpectSuccess program VM.nil
        , Test.test "Returns Constant in a LDC program" <|
            \_ ->
                let
                    program =
                        Prog.fromSingleton (LDC 5)
                in
                vmExpectSuccess program (VM.Integer 5)
        ]



-- build in operations such as ADDition


testBuiltIns : Test
testBuiltIns =
    Test.describe "Tests built in operations"
        [ testNumeric
        , testAtom
        , testCons
        ]


testNumeric : Test
testNumeric =
    Test.describe "Tests numeric functions"
        [ Test.fuzz fuzzIntPair "Adds two numbers correctly" <|
            \( a, b ) ->
                let
                    program =
                        Prog.fromList [ LDC a, LDC b, FUNC ADD ]
                in
                vmExpectSuccess program (VM.Integer (a + b))
        , Test.fuzz fuzzIntPair "Multiplies two numbers correctly" <|
            \( a, b ) ->
                let
                    program =
                        Prog.fromList [ LDC a, LDC b, FUNC MULT ]
                in
                vmExpectSuccess program (VM.Integer (a * b))
        , Test.test "Addition fails when types are wrong" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC ADD ]
                in
                vmExpectFailure program
        ]


testAtom : Test
testAtom =
    Test.describe "Tests the atom function (returns true when the value is ATOMic)"
        [ Test.test "Atom function correctly identifies a number as an Atom" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, FUNC ATOM ]
                in
                vmExpectSuccess program (VM.Boolean True)
        , Test.test "Atom function fails on empty stack" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ FUNC ATOM ]
                in
                vmExpectFailure program
        ]


testCons : Test
testCons =
    Test.describe "Test Cons functionality"
        [ Test.test "Cons a single element" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS ]
                in
                vmExpectSuccess program (VM.Array <| Cons.fromList [ VM.Integer 5 ])
        , Test.test "Cons two elements" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, LDC 6, FUNC CONS ]
                in
                vmExpectSuccess program (VM.Array <| Cons.fromList [ VM.Integer 6, VM.Integer 5 ])
        , Test.test "Fails when consing a nil" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, LDC 6, FUNC CONS ]
                in
                vmExpectSuccess program (VM.Array <| Cons.fromList [ VM.Integer 6, VM.Integer 5 ])
        ]



-- if/else control flow


testIfElse : Test
testIfElse =
    Test.describe "Tests if/else control flow"
        [ Test.test "Correctly chooses the left side" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, FUNC ATOM, SEL, NESTED [ LDC 1, JOIN ], NESTED [ LDC 2, JOIN ] ]
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
