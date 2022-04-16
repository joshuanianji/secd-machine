module SECD.VMSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Lib.Cons as Cons
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..), Program)
import SECD.VM as VM exposing (Value)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Test VM"
        [ testBasics
        , testBuiltIns
        , testIfElse
        , testFuncs
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
        , testCompare
        ]


testNumeric : Test
testNumeric =
    Test.describe "Tests numeric functions"
        [ Test.fuzz2 Fuzz.int Fuzz.int "Adds two numbers correctly" <|
            \a b ->
                let
                    program =
                        Prog.fromList [ LDC a, LDC b, FUNC ADD ]
                in
                vmExpectSuccess program (VM.Integer (a + b))
        , Test.fuzz2 Fuzz.int Fuzz.int "Multiplies two numbers correctly" <|
            \a b ->
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
        , Test.test "(+ (* 3 5) (* 6 8)) = 63" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 8, LDC 6, FUNC MULT, LDC 5, LDC 3, FUNC MULT, FUNC ADD ]
                in
                vmExpectSuccess program (VM.Integer 63)
        ]


testAtom : Test
testAtom =
    Test.describe "Tests the atom function (returns true when the value is atomic)"
        [ Test.test "True on a number" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, FUNC ATOM ]
                in
                vmExpectSuccess program (VM.Boolean True)
        , Test.test "True on a boolean" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, FUNC ATOM, FUNC ATOM ]
                in
                vmExpectSuccess program (VM.Boolean True)
        , Test.test "Fails on an empty stack" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ FUNC ATOM ]
                in
                vmExpectFailure program
        , Test.test "False on a list" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, FUNC ATOM ]
                in
                vmExpectSuccess program (VM.Boolean False)
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


testCompare : Test
testCompare =
    Test.describe "Test comparing elements" <|
        List.map
            (\cmp ->
                Test.fuzz2 Fuzz.int Fuzz.int ("Compare OP: " ++ Prog.cmpToString cmp) <|
                    \a b ->
                        let
                            program =
                                Prog.fromList [ LDC a, LDC b, FUNC <| COMPARE cmp ]
                        in
                        vmExpectSuccess program (VM.Boolean <| Prog.cmpFunc cmp b a)
            )
            [ CMP_EQ, CMP_NE, CMP_LT, CMP_GT, CMP_LEQ, CMP_GEQ ]



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
        , Test.test "Correctly chooses the right side" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, FUNC ATOM, SEL, NESTED [ LDC 1, JOIN ], NESTED [ LDC 2, JOIN ] ]
                in
                vmExpectSuccess program (VM.Integer 2)
        ]



-- non recursive functions


testFuncs : Test
testFuncs =
    Test.describe "Test Non-Recursive Functions"
        [ Test.fuzz2 Fuzz.int Fuzz.int "Function that adds two numbers" <|
            \a b ->
                let
                    -- ((lambda (x y) (+ x y)) a b)
                    program =
                        Prog.fromList [ NIL, LDC a, FUNC CONS, LDC b, FUNC CONS, LDF, NESTED [ LD ( 0, 1 ), LD ( 0, 0 ), FUNC ADD, RTN ], AP ]
                in
                vmExpectSuccess program (VM.Integer (a + b))
        , Test.fuzz2 Fuzz.int Fuzz.int "Function that multiplies two numbers" <|
            \a b ->
                let
                    -- ((lambda (x y) (* x y)) a b)
                    program =
                        Prog.fromList [ NIL, LDC a, FUNC CONS, LDC b, FUNC CONS, LDF, NESTED [ LD ( 0, 1 ), LD ( 0, 0 ), FUNC MULT, RTN ], AP ]
                in
                vmExpectSuccess program (VM.Integer (a * b))
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "3-ary function that returns a list containing its arguments" <|
            \a b c ->
                let
                    createList =
                        [ NIL, LDC a, FUNC CONS, LDC b, FUNC CONS, LDC c, FUNC CONS ]

                    funcBody =
                        [ NIL, LD ( 0, 0 ), FUNC CONS, LD ( 0, 1 ), FUNC CONS, LD ( 0, 2 ), FUNC CONS, RTN ]

                    -- ((lambda (x y z) (cons x (cons y (cons z nil))) a b c)
                    program =
                        Prog.fromList <| createList ++ [ LDF, NESTED funcBody, AP ]
                in
                vmExpectSuccess program <| VM.Array <| Cons.fromList [ VM.Integer a, VM.Integer b, VM.Integer c ]
        , Test.fuzz2 Fuzz.int Fuzz.int "2-ary function that returns the larger element" <|
            \a b ->
                let
                    -- ((lambda (x y) (if (< x y) y x)) a b)
                    funcBody =
                        [ LD ( 0, 1 ), LD ( 0, 0 ), FUNC (COMPARE CMP_LT), SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ LD ( 0, 0 ), JOIN ], RTN ]

                    program =
                        Prog.fromList [ NIL, LDC a, FUNC CONS, LDC b, FUNC CONS, LDF, NESTED funcBody, AP ]
                in
                vmExpectSuccess program (VM.Integer (max a b))
        ]



-- Helpers


vmExpectSuccess : Program -> Value -> Expectation
vmExpectSuccess prog expected =
    Expect.equal (VM.evaluate <| VM.init prog) (Ok expected)


vmExpectFailure : Program -> Expectation
vmExpectFailure prog =
    Expect.err (VM.evaluate <| VM.init prog)
