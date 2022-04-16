module SECD.EnvironmentSpec exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import SECD.Environment exposing (..)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Environment Stack"
        [ testLocate ]


testLocate : Test
testLocate =
    Test.describe "Environment.locate"
        [ Test.test "Locates (0,0)" <|
            \_ ->
                let
                    env =
                        [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (locate ( 0, 0 ) env) (Ok 1)
        , Test.test "Locates (1,1)" <|
            \_ ->
                let
                    env =
                        [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (locate ( 1, 1 ) env) (Ok 4)
        , Test.test "Locates (1,0)" <|
            \_ ->
                let
                    env =
                        [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (locate ( 1, 0 ) env) (Ok 3)
        , Test.test "Fails in out of bound range" <|
            \_ ->
                let
                    env =
                        [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.err (locate ( 2, 1 ) env)
        , Test.fuzz (fuzzIntPairRange 2 10) "fails in out of bound range (fuzz)" <|
            \( x, y ) ->
                let
                    env =
                        [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.err (locate ( x, y ) env)
        , Test.fuzz (fuzzIntPairRange -10 -1) "Fails at negative indices" <|
            \( x, y ) ->
                let
                    env =
                        [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.err (locate ( x, y ) env)
        ]


fuzzIntPairRange : Int -> Int -> Fuzzer ( Int, Int )
fuzzIntPairRange start end =
    Fuzz.tuple ( Fuzz.intRange start end, Fuzz.intRange start end )
