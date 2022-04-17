module SECD.VMSpec exposing (suite)

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
        , testRecursiveFuncs
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
        , testCarCdr
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
                vmExpectSuccess program VM.Truthy
        , Test.test "True on a boolean" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, FUNC ATOM, FUNC ATOM ]
                in
                vmExpectSuccess program VM.Truthy
        , Test.test "Fails on an empty stack" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ FUNC ATOM ]
                in
                vmExpectFailure program
        , Test.test "Nil on a list" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, FUNC ATOM ]
                in
                vmExpectSuccess program VM.nil
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
        , Test.test "Cons nested list - ((1 2) 3)" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 3, FUNC CONS, NIL, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC CONS ]
                in
                case VM.evaluate <| VM.init program of
                    Result.Ok (VM.Array cons) ->
                        Expect.equal (Cons.toString cons VM.valueToString) "((1 2) 3)"

                    _ ->
                        Expect.fail "Failed to evaluate program"
        ]


testCarCdr : Test
testCarCdr =
    Test.describe "Test Car and Cdr"
        [ Test.test "Car on NIL returns NIL" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, FUNC CAR ]
                in
                vmExpectSuccess program VM.nil
        , Test.test "Car on a list returns the first element" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, LDC 6, FUNC CONS, FUNC CAR ]
                in
                vmExpectSuccess program (VM.Integer 6)
        , Test.test "Car on an integer fails" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, FUNC CAR ]
                in
                vmExpectFailure program
        , Test.test "Cdr on a list returns the rest of the list" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, LDC 6, FUNC CONS, LDC 7, FUNC CONS, FUNC CDR ]
                in
                vmExpectSuccess program (VM.Array <| Cons.fromList [ VM.Integer 6, VM.Integer 5 ])
        , Test.test "Cdr on an integer fails" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ LDC 5, FUNC CDR ]
                in
                vmExpectFailure program
        , Test.test "Cdr on NIL returns NIL" <|
            \_ ->
                let
                    program =
                        Prog.fromList [ NIL, FUNC CDR ]
                in
                vmExpectSuccess program VM.nil
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
                        vmExpectSuccess program (VM.boolToValue <| Prog.cmpFunc cmp b a)
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
        , Test.test "Constant function that returns an int" <|
            \_ ->
                let
                    -- ((lambda (x) 3) 5)
                    program =
                        Prog.fromList [ NIL, LDC 5, FUNC CONS, LDF, NESTED [ LDC 3 ], AP ]
                in
                vmExpectSuccess program (VM.Integer 3)
        , Test.test "Function inside a function" <|
            \_ ->
                let
                    -- (lambda (x y) (+ (- x y) z)
                    innerFunc =
                        [ LD ( 1, 0 ), LD ( 0, 1 ), LD ( 0, 0 ), FUNC SUB, FUNC ADD, RTN ]

                    -- (lambda (z) ((lambda (x y) (+ (- x y) z)) 3 5)
                    outerFunc =
                        [ NIL, LDC 5, FUNC CONS, LDC 3, FUNC CONS, LDF, NESTED innerFunc, AP, RTN ]

                    -- ((lambda (z) ((lambda (x y) (+ (- x y) z)) 3 5)) 6)
                    program =
                        Prog.fromList [ NIL, LDC 6, FUNC CONS, LDF, NESTED outerFunc, AP ]
                in
                vmExpectSuccess program (VM.Integer 4)
        ]


testRecursiveFuncs : Test
testRecursiveFuncs =
    Test.describe "Recursive Functions"
        [ Test.test "Recursive sum function" <|
            \_ ->
                let
                    -- f = (λx m | (if (null x) m (f (cdr x) (+ m 1) )) )
                    func =
                        [ LD ( 0, 0 ), FUNC NULL, SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ NIL, LDC 1, LD ( 0, 1 ), FUNC ADD, FUNC CONS, LD ( 0, 0 ), FUNC CDR, FUNC CONS, LD ( 1, 0 ), AP, JOIN ], RTN ]

                    -- (f '(1 2 3) 0)
                    funcApply =
                        [ NIL, LDC 0, FUNC CONS, NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC CONS, LD ( 0, 0 ), AP, RTN ]

                    program =
                        Prog.fromList
                            [ DUM, NIL, LDF, NESTED func, FUNC CONS, LDF, NESTED funcApply, RAP ]
                in
                vmExpectSuccess program (VM.Integer 6)
        ]



-- Helpers


vmExpectSuccess : Program -> Value -> Expectation
vmExpectSuccess prog expected =
    Expect.equal (VM.evaluate <| VM.init prog) (Ok expected)


vmExpectFailure : Program -> Expectation
vmExpectFailure prog =
    Expect.err (VM.evaluate <| VM.init prog)
