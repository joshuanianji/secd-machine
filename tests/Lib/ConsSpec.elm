module Lib.ConsSpec exposing (suite)

import Expect
import Fuzz
import Json.Decode as Decode
import Json.Encode as Encode
import Lib.Cons exposing (..)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Test Cons"
        [ testFromList
        , testFromConsList
        , testToString
        , testToList
        , testDecode
        ]


testFromList : Test
testFromList =
    Test.describe "Cons.fromList"
        [ Test.test "[] -> Nil" <|
            \_ ->
                let
                    actual =
                        fromList []

                    expected =
                        Nil
                in
                Expect.equal actual expected
        , Test.test "[a,b,c,d,e]" <|
            \_ ->
                let
                    actual =
                        fromList [ "a", "b", "c", "d", "e" ]

                    expectedCons =
                        Cons (Val "a") (Cons (Val "b") (Cons (Val "c") (Cons (Val "d") (Cons (Val "e") Nil))))
                in
                Expect.equal actual expectedCons
        , Test.fuzz (Fuzz.list Fuzz.int) "Random integer lists" <|
            \xs ->
                let
                    actual =
                        fromList xs

                    expectedCons =
                        List.foldr (\x acc -> Cons (Val x) acc) Nil xs
                in
                Expect.equal actual expectedCons
        ]


testFromConsList : Test
testFromConsList =
    Test.describe "Cons.fromConsList"
        [ Test.test "[] -> Nil" <|
            \_ ->
                let
                    actual =
                        fromConsList []

                    expected =
                        Nil
                in
                Expect.equal actual expected
        , Test.test "[Nil]" <|
            \_ ->
                let
                    actual =
                        fromConsList [ Nil ]

                    expectedCons =
                        Cons Nil Nil
                in
                Expect.equal actual expectedCons
        , Test.test "((a b) (c d) (e f g) h)" <|
            \_ ->
                let
                    actual =
                        fromConsList [ fromList [ "a", "b" ], fromList [ "c", "d" ], fromList [ "e", "f", "g" ], Val "h" ]

                    expectedCons =
                        Cons (Cons (Val "a") (Cons (Val "b") Nil)) (Cons (Cons (Val "c") (Cons (Val "d") Nil)) (Cons (Cons (Val "e") (Cons (Val "f") (Cons (Val "g") Nil))) (Cons (Val "h") Nil)))
                in
                Expect.equal actual expectedCons
        ]


testToString : Test
testToString =
    Test.describe "Cons.toString"
        [ simpleToString
        , simpleWithDots
        , nested
        ]


simpleToString : Test
simpleToString =
    Test.describe "Simple tests"
        [ Test.test "Cons.toString (a)" <|
            \_ ->
                let
                    cons =
                        Cons (Val "a") Nil
                in
                Expect.equal (toString identity cons) "(a)"
        , Test.test "Cons.toString (a b)" <|
            \_ ->
                let
                    cons =
                        fromList [ "a", "b" ]
                in
                Expect.equal (toString identity cons) "(a b)"
        , Test.test "Cons.toString (a b c d)" <|
            \_ ->
                let
                    cons =
                        fromList [ "a", "b", "c", "d" ]
                in
                Expect.equal (toString identity cons) "(a b c d)"
        ]


simpleWithDots : Test
simpleWithDots =
    Test.describe "Simple tests with dots (or that aren't perfect lists)"
        [ Test.test "Cons.toString (a . b)" <|
            \_ ->
                let
                    cons =
                        Cons (Val "a") (Val "b")
                in
                Expect.equal (toString identity cons) "(a . b)"
        , Test.test "Cons.toString (a b c . d)" <|
            \_ ->
                let
                    cons =
                        Cons (Val "a") (Cons (Val "b") (Cons (Val "c") (Val "d")))
                in
                Expect.equal (toString identity cons) "(a b c . d)"
        , Test.test "Cons.toString ((a . b))" <|
            \_ ->
                let
                    cons =
                        Cons (Cons (Val "a") (Val "b")) Nil
                in
                Expect.equal (toString identity cons) "((a . b))"
        ]


nested : Test
nested =
    Test.describe "Nested or deeply nested"
        [ Test.test "Cons.toString ((a b) (c d) e)" <|
            \_ ->
                let
                    cons =
                        fromConsList [ fromList [ "a", "b" ], fromList [ "c", "d" ], Val "e" ]
                in
                Expect.equal (toString identity cons) "((a b) (c d) e)"
        , Test.test "Cons.toString ((((a))))" <|
            \_ ->
                let
                    cons =
                        fromConsList [ fromConsList [ fromConsList [ fromList [ "a" ] ] ] ]
                in
                Expect.equal (toString identity cons) "((((a))))"
        , Test.test "Cons.toString (((a . b)))" <|
            \_ ->
                let
                    cons =
                        Cons (Cons (Cons (Val "a") (Val "b")) Nil) Nil
                in
                Expect.equal (toString identity cons) "(((a . b)))"
        ]


testToList : Test
testToList =
    Test.describe "Cons.toList"
        [ Test.test "Nil -> []" <|
            \_ ->
                let
                    actual =
                        toList Nil

                    expected =
                        Just []
                in
                Expect.equal actual expected
        , Test.test "[1,2,3]" <|
            \_ ->
                let
                    actual =
                        toList (fromList [ 1, 2, 3 ])

                    expected =
                        Just [ Val 1, Val 2, Val 3 ]
                in
                Expect.equal actual expected
        , Test.test "Works on a nested list" <|
            \_ ->
                let
                    actual =
                        toList (fromConsList [ fromList [ 1, 2 ], Val 3 ])

                    expected =
                        Just [ fromList [ 1, 2 ], Val 3 ]
                in
                Expect.equal actual expected
        , Test.test "Fails when the life does not end in Nil" <|
            \_ ->
                let
                    actual =
                        toList (Cons (Val 1) (Val 2))

                    expected =
                        Nothing
                in
                Expect.equal actual expected
        ]


testDecode : Test
testDecode =
    Test.describe "Cons.decode" <|
        List.map
            (\cons ->
                Test.test ("Decode: " ++ toString String.fromInt cons) <|
                    \_ ->
                        Decode.decodeValue (decoder Decode.int) (encode Encode.int cons)
                            |> Expect.equal (Ok cons)
            )
            [ Nil
            , Val 2
            , Cons (Val 1) (Val 2)
            , Cons (Val 1) (Cons (Val 2) (Val 3))
            , Cons (Val 1) (Cons (Val 2) (Cons (Val 3) (Val 4)))
            , Cons (Cons (Val 2) (Val 10)) Nil
            , fromList [ 4, 5, 6, 7, 8 ]
            ]
