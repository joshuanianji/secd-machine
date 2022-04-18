module SECD.EnvironmentSpec exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Lib.Cons as Cons
import SECD.Environment as Env
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Environment Stack"
        [ testLocate
        , testPush
        , testPushDummy
        , testReplaceDummy
        ]


testLocate : Test
testLocate =
    Test.describe "Environment.locate"
        [ Test.test "Locates (0,0)" <|
            \_ ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Env.locate ( 0, 0 ) Nothing env) (Ok <| Cons.Val 1)
        , Test.test "Locates (1,1)" <|
            \_ ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Env.locate ( 1, 1 ) Nothing env) (Ok <| Cons.Val 4)
        , Test.test "Locates (1,0)" <|
            \_ ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Env.locate ( 1, 0 ) Nothing env) (Ok <| Cons.Val 3)
        , Test.test "Fails in out of bound range" <|
            \_ ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.err (Env.locate ( 2, 1 ) Nothing env)
        , Test.fuzz (fuzzIntPairRange 2 10) "fails in out of bound range (fuzz)" <|
            \( x, y ) ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.err (Env.locate ( x, y ) Nothing env)
        , Test.fuzz (fuzzIntPairRange -10 -1) "Fails at negative indices" <|
            \( x, y ) ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.err (Env.locate ( x, y ) Nothing env)
        , Test.test "Fails with uniniitalized dummy value" <|
            \_ ->
                let
                    env =
                        [ Env.Dummy ]
                in
                Expect.err (Env.locate ( 0, 0 ) Nothing env)
        , Test.test "Returns dummy value when dummy is present" <|
            \_ ->
                let
                    env =
                        [ Env.Dummy ]
                in
                Expect.equal (Env.locate ( 0, 0 ) (Just [ Cons.Val 1 ]) env) (Ok <| Cons.Val 1)
        ]


testPush : Test
testPush =
    Test.describe "Environment.push"
        [ Test.test "Push on an empty stack" <|
            \_ ->
                let
                    env =
                        Env.fromList []
                in
                Expect.equal (Env.push [ Cons.Val 0, Cons.Val 0 ] env) (Env.fromList [ [ 0, 0 ] ])
        , Test.test "Push on a non-empty stack" <|
            \_ ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Env.push [ Cons.Val 0, Cons.Val 0 ] env) (Env.fromList [ [ 0, 0 ], [ 1, 2 ], [ 3, 4 ] ])
        ]


testPushDummy : Test
testPushDummy =
    Test.describe "Environment.pushDummy"
        [ Test.test "Push on an empty stack" <|
            \_ ->
                let
                    env =
                        Env.fromList []
                in
                Expect.equal (Env.pushDummy env) [ Env.Dummy ]
        , Test.test "Push on a non-empty stack" <|
            \_ ->
                let
                    env =
                        Env.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Env.pushDummy env) [ Env.Dummy, Env.ListItem [ Cons.Val 1, Cons.Val 2 ], Env.ListItem [ Cons.Val 3, Cons.Val 4 ] ]
        ]


testReplaceDummy : Test
testReplaceDummy =
    Test.describe "Environment.rplaca"
        [ Test.test "Replaces the first row in a nonempty env" <|
            \_ ->
                let
                    env =
                        [ Env.Dummy, Env.ListItem [ Cons.Val 3, Cons.Val 4 ] ]

                    expected =
                        Env.fromList [ [ 5, 6 ], [ 3, 4 ] ]
                in
                Expect.equal (Env.replaceDummy [ Cons.Val 5, Cons.Val 6 ] env) (Ok expected)
        , Test.test "Fails on an empty environment" <|
            \_ ->
                let
                    env =
                        []
                in
                Expect.err (Env.replaceDummy [ Cons.Val 5, Cons.Val 6 ] env)
        , Test.test "Fails without a dummy" <|
            \_ ->
                let
                    env =
                        [ Env.ListItem [ Cons.Val 3, Cons.Val 4 ] ]
                in
                Expect.err (Env.replaceDummy [ Cons.Val 5, Cons.Val 6 ] env)
        ]


fuzzIntPairRange : Int -> Int -> Fuzzer ( Int, Int )
fuzzIntPairRange start end =
    Fuzz.tuple ( Fuzz.intRange start end, Fuzz.intRange start end )
