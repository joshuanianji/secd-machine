module SECD.VMSpec exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import SECD.Program as Prog exposing (Func(..), Op(..))
import SECD.VM as VM
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Test VM"
        [ testBasics
        , testBuiltIns
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
                Expect.equal (VM.evaluate <| VM.init program) (Ok VM.Nil)
        , Test.test "Returns Constant in a LDC program" <|
            \_ ->
                let
                    program =
                        Prog.fromSingleton (LDC 5)
                in
                Expect.equal (VM.evaluate <| VM.init program) (Ok <| VM.Integer 5)
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
                Expect.equal (VM.evaluate <| VM.init program) (Ok <| VM.Integer (a + b))
        , Test.fuzz fuzzIntPair "Multiplies two numbers correctly" <|
            \( a, b ) ->
                let
                    program =
                        Prog.fromList [ LDC a, LDC b, Func Mult ]
                in
                Expect.equal (VM.evaluate <| VM.init program) (Ok <| VM.Integer (a * b))
        , Test.test "Addition fails when types are wrong" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, Func Add ]
                in
                Expect.err (VM.evaluate <| VM.init program)
        ]


fuzzIntPair : Fuzzer ( Int, Int )
fuzzIntPair =
    Fuzz.tuple ( Fuzz.int, Fuzz.int )
