module SECD.VMSpec exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import Json.Decode as Decode
import Lib.Cons as Cons
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..), Program)
import SECD.VM as VM exposing (Value)
import SECD.VMEnv as Env
import Test exposing (Test)



-- REUSABLE PROGRAMS


recursiveLength : Prog.Program
recursiveLength =
    let
        -- f = (λx m | (if (null x) m (f (cdr x) (+ m 1) )) )
        func =
            [ LD ( 0, 0 ), FUNC NULL, SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ NIL, LDC 1, LD ( 0, 1 ), FUNC ADD, FUNC CONS, LD ( 0, 0 ), FUNC CDR, FUNC CONS, LD ( 1, 0 ), AP, JOIN ], RTN ]

        -- (f '(1 2 3) 0)
        funcApply =
            [ NIL, LDC 0, FUNC CONS, NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC CONS, LD ( 0, 0 ), AP, RTN ]
    in
    Prog.fromList
        [ DUM, NIL, LDF, NESTED func, FUNC CONS, LDF, NESTED funcApply, RAP ]


factorial : Int -> Prog.Program
factorial n =
    let
        fact =
            [ LDC 0, LD ( 0, 0 ), FUNC (COMPARE CMP_EQ), SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ NIL, LD ( 0, 1 ), LD ( 0, 0 ), FUNC MULT, FUNC CONS, LD ( 2, 1 ), LD ( 0, 0 ), FUNC SUB, FUNC CONS, LD ( 1, 0 ), AP, JOIN ], RTN ]

        -- I actually don't really know what this does
        factCreateClosure =
            [ NIL, LD ( 1, 1 ), FUNC CONS, LD ( 1, 0 ), FUNC CONS, LD ( 0, 0 ), AP, RTN ]
    in
    Prog.fromList [ NIL, LDC 1, FUNC CONS, LDC n, FUNC CONS, LDF, NESTED [ DUM, NIL, LDF, NESTED fact, FUNC CONS, LDF, NESTED factCreateClosure, RAP, RTN ], AP ]


mutualRecursiveIsEven : Int -> Prog.Program
mutualRecursiveIsEven n =
    let
        isEven =
            mutualRecursive [ NIL ] ( 1, 1 )

        isOdd =
            mutualRecursive [ NIL, FUNC ATOM ] ( 1, 0 )

        mutualRecursive onTrue letrecCoords =
            [ LDC 0, LD ( 0, 0 ), FUNC (COMPARE CMP_EQ), SEL, NESTED <| onTrue ++ [ JOIN ], NESTED [ NIL, LDC 1, LD ( 0, 0 ), FUNC SUB, FUNC CONS, LD letrecCoords, AP, JOIN ], RTN ]

        body =
            [ NIL, LDC n, FUNC CONS, LD ( 0, 1 ), AP, RTN ]
    in
    Prog.fromList
        [ DUM, NIL, LDF, NESTED isOdd, FUNC CONS, LDF, NESTED isEven, FUNC CONS, LDF, NESTED body, RAP ]



-- TESTS


suite : Test
suite =
    Test.describe "Test VM"
        [ testBasics
        , testBuiltIns
        , testIfElse
        , testFuncs
        , testRecursiveFuncs
        , testEvalFuncs
        , testDecoder
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
                case VM.evaluate <| VM.initRaw program of
                    ( _, Result.Ok (VM.Array cons) ) ->
                        Expect.equal (Cons.toString VM.valueToString cons) "((1 2) 3)"

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
        , Test.test "Applying a function as an argument (square)" <|
            \_ ->
                let
                    square =
                        [ LD ( 0, 0 ), LD ( 0, 0 ), FUNC MULT, RTN ]

                    squareApply =
                        [ NIL, LDC 3, FUNC CONS, LD ( 0, 0 ), AP, RTN ]

                    program =
                        Prog.fromList [ NIL, LDF, NESTED square, FUNC CONS, LDF, NESTED squareApply, AP ]
                in
                vmExpectSuccess program (VM.Integer 9)
        ]


testRecursiveFuncs : Test
testRecursiveFuncs =
    Test.describe "Recursive Functions"
        [ Test.test "Recursive list length function" <|
            \_ ->
                vmExpectSuccess recursiveLength (VM.Integer 3)
        , Test.test "Factorial function" <|
            \_ ->
                vmExpectSuccess (factorial 6) (VM.Integer 720)
        , Test.fuzzWith { runs = 5 } (Fuzz.intRange 0 10) "Mutually recursive isEven" <|
            \n ->
                let
                    expected =
                        if modBy 2 n == 1 then
                            VM.nil

                        else
                            VM.Truthy
                in
                vmExpectSuccess (mutualRecursiveIsEven n) expected
        ]



-- EVAL FUNCS


testEvalFuncs : Test
testEvalFuncs =
    Test.describe "Evaluation functions"
        [ testEvalPage
        , testGetPages
        ]


testEvalPage : Test
testEvalPage =
    Test.describe "evalPage" <|
        List.map
            (\( name, prog ) ->
                Test.test name <|
                    \_ ->
                        let
                            -- the expected steps is one less than the expected states
                            -- since a step goes between two states
                            expectedSteps =
                                VM.initRaw prog
                                    |> VM.evaluate
                                    |> Tuple.first

                            -- the size when n=8 is 99 steps, so make sure the page can hold more than that
                            evalPageLength =
                                VM.initRaw prog
                                    |> VM.evalPage 15 15
                                    |> .totalVMCount
                        in
                        Expect.equal evalPageLength (expectedSteps + 1)
            )
            [ -- Ensure all these programs terminate within one page
              ( "Basic arithmetic", Prog.fromList [ LDC 4, LDC 3, FUNC ADD, LDC 2, LDC 1, FUNC ADD, FUNC MULT ] )
            , ( "Basic if statement"
              , Prog.fromList [ LDC 2, LDC 1, FUNC (COMPARE CMP_GT), SEL, NESTED [ LDC 1, JOIN ], NESTED [ LDC 2, JOIN ] ]
              )
            , ( "Basic Let Statements"
              , Prog.fromList [ NIL, LDC 1, FUNC CONS, LDC 5, FUNC CONS, LDF, NESTED [ NIL, LDC 2, FUNC CONS, LDF, NESTED [ NIL, LDC 3, FUNC CONS, LDF, NESTED [ LD ( 0, 0 ), LD ( 2, 0 ), FUNC ADD, LD ( 1, 0 ), LD ( 2, 1 ), FUNC ADD, FUNC ADD, RTN ], AP, RTN ], AP, RTN ], AP ]
              )
            , ( "isEven 2", mutualRecursiveIsEven 2 )
            , ( "isEven 6", mutualRecursiveIsEven 6 )
            , ( "factorial 3", factorial 3 )
            , ( "factorial 7", factorial 7 )
            , ( "recursiveLength", recursiveLength )
            ]


testGetPages : Test
testGetPages =
    Test.describe "getPages" <|
        List.map
            (\( name, prog ) ->
                Test.test name <|
                    \_ ->
                        let
                            -- the expected steps is one less than the expected states
                            -- since a step goes between two states
                            expectedSteps =
                                VM.initRaw prog
                                    |> VM.evaluate
                                    |> Tuple.first

                            evalPageLength =
                                VM.initRaw prog
                                    |> VM.getPages { maxPages = 10, pageSize = 4, chunkSize = 5 }
                                    |> .totalVMCount
                        in
                        Expect.equal evalPageLength (expectedSteps + 1)
            )
            [ ( "Basic arithmetic", Prog.fromList [ LDC 4, LDC 3, FUNC ADD ] )
            , ( "Basic arithmetic 2", Prog.fromList [ LDC 4, LDC 3, FUNC ADD, LDC 2, LDC 1, FUNC ADD, FUNC MULT ] )
            , ( "isEven 6", mutualRecursiveIsEven 6 )
            , ( "factorial 10", factorial 10 )
            , ( "factorial 7", factorial 7 )
            , ( "recursiveLength", recursiveLength )
            ]



-- DECODE/ENCODE


testDecoder : Test
testDecoder =
    Test.describe "Decoder/encoder for VM"
        [ testValueDecoder
        , testWithPrograms
        ]


testValueDecoder : Test
testValueDecoder =
    Test.describe "Decoder/encoder for VM Value type"
        [ Test.fuzz Fuzz.int "integer" <|
            \n ->
                VM.Integer n
                    |> VM.encodeValue
                    |> Decode.decodeValue VM.valueDecoder
                    |> Expect.equal (Ok <| VM.Integer n)
        , Test.test "Truthy" <|
            \_ ->
                VM.encodeValue VM.Truthy
                    |> Decode.decodeValue VM.valueDecoder
                    |> Expect.equal (Ok <| VM.Truthy)
        , Test.test "Empty Array" <|
            \_ ->
                VM.Array Cons.nil
                    |> VM.encodeValue
                    |> Decode.decodeValue VM.valueDecoder
                    |> Expect.equal (Ok <| VM.Array Cons.nil)
        , Test.test "Nonempty Array" <|
            \_ ->
                (VM.Array <| Cons.fromList [ VM.Integer 1, VM.Integer 2, VM.Integer 3 ])
                    |> VM.encodeValue
                    |> Decode.decodeValue VM.valueDecoder
                    |> Expect.equal (Ok <| VM.Array <| Cons.fromList [ VM.Integer 1, VM.Integer 2, VM.Integer 3 ])
        , Test.test "Empty Closure" <|
            \_ ->
                VM.Closure [] Env.init
                    |> VM.encodeValue
                    |> Decode.decodeValue VM.valueDecoder
                    |> Expect.equal (Ok <| VM.Closure [] Env.init)
        , Test.test "Nonempty closure" <|
            \_ ->
                VM.Closure [ NIL ] Env.init
                    |> VM.encodeValue
                    |> Decode.decodeValue VM.valueDecoder
                    |> Expect.equal (Ok <| VM.Closure [ NIL ] Env.init)
        ]


testWithPrograms : Test
testWithPrograms =
    Test.describe "VM Decoder/encoder with real programs"
        [ Test.fuzzWith { runs = 5 } (Fuzz.intRange 5 10) "Recursive list length function" <|
            \steps ->
                let
                    stepped =
                        VM.initRaw recursiveLength
                            |> VM.stepN steps
                in
                case stepped of
                    VM.Unfinished vm ->
                        Expect.equal (Decode.decodeValue VM.decoder <| VM.encode vm) (Ok vm)

                    VM.Finished _ _ ->
                        Expect.fail <| "Rec. Length program finished prematurely in " ++ String.fromInt steps ++ " steps!"

                    VM.Error _ _ ->
                        Expect.fail <| "Rec. Length program errored out!"
        , Test.fuzzWith { runs = 5 } (Fuzz.intRange 5 10) "Factorial Function" <|
            \steps ->
                let
                    stepped =
                        VM.initRaw (factorial 6)
                            |> VM.stepN steps
                in
                case stepped of
                    VM.Unfinished vm ->
                        Expect.equal (Decode.decodeValue VM.decoder <| VM.encode vm) (Ok vm)

                    VM.Finished _ _ ->
                        Expect.fail <| "Factorial program finished prematurely in " ++ String.fromInt steps ++ " steps!"

                    VM.Error _ _ ->
                        Expect.fail <| "Factorial program errored out!"
        ]



-- Helpers


vmExpectSuccess : Program -> Value -> Expectation
vmExpectSuccess prog expected =
    VM.initRaw prog
        |> VM.evaluate
        |> Tuple.second
        |> Expect.equal (Ok expected)


vmExpectFailure : Program -> Expectation
vmExpectFailure prog =
    VM.initRaw prog
        |> VM.evaluate
        |> Tuple.second
        |> Expect.err
