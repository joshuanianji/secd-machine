module Lib.ConsSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Lib.Cons exposing (..)
import Test exposing (Test)



-- test cons print


suite : Test
suite =
    Test.describe "Test Cons"
        [ testFromList
        , testFromConsList
        , testToString
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
                Expect.equal (toString cons identity) "(a)"
        , Test.test "Cons.toString (a b)" <|
            \_ ->
                let
                    cons =
                        fromList [ "a", "b" ]
                in
                Expect.equal (toString cons identity) "(a b)"
        , Test.test "Cons.toString (a b c d)" <|
            \_ ->
                let
                    cons =
                        fromList [ "a", "b", "c", "d" ]
                in
                Expect.equal (toString cons identity) "(a b c d)"
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
                Expect.equal (toString cons identity) "(a . b)"
        , Test.test "Cons.toString (a b c . d)" <|
            \_ ->
                let
                    cons =
                        Cons (Val "a") (Cons (Val "b") (Cons (Val "c") (Val "d")))
                in
                Expect.equal (toString cons identity) "(a b c . d)"
        , Test.test "Cons.toString ((a . b))" <|
            \_ ->
                let
                    cons =
                        Cons (Cons (Val "a") (Val "b")) Nil
                in
                Expect.equal (toString cons identity) "((a . b))"
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
                Expect.equal (toString cons identity) "((a b) (c d) e)"
        , Test.test "Cons.toString ((((a))))" <|
            \_ ->
                let
                    cons =
                        fromConsList [ fromConsList [ fromConsList [ fromList [ "a" ] ] ] ]
                in
                Expect.equal (toString cons identity) "((((a))))"
        , Test.test "Cons.toString ((a . b))" <|
            \_ ->
                let
                    cons =
                        Cons (Cons (Val "a") (Val "b")) Nil
                in
                Expect.equal (toString cons identity) "((a . b))"
        ]