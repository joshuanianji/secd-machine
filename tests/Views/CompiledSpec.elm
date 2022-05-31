module Views.CompiledSpec exposing (suite)

import Expect
import Lib.LispAST as AST
import Lib.Util as Util
import List.Extra
import Programs
import SECD.Program as Prog
import Test exposing (Test)
import Views.Compiled exposing (..)


suite : Test
suite =
    Test.describe "compiledSpec"
        [ testTranspiler
        , testTranspilerIndices
        ]


testTranspiler : Test
testTranspiler =
    Test.describe "transpiler"
        [ transpilerTrivials
        , transpilerLDF
        ]


transpilerTrivials : Test
transpilerTrivials =
    Test.describe "trivials" <|
        List.map
            (\( op, expected ) ->
                Test.test (Prog.opToString op) <|
                    \_ ->
                        transpile [ op ]
                            |> Result.map (List.map stripIndices)
                            |> Expect.equal (Ok [ Unindexed expected ])
            )
            [ ( Prog.NIL, NIL )
            , ( Prog.LD ( 0, 0 ), LD ( 0, 0 ) )
            , ( Prog.LDC 5, LDC 5 )
            , ( Prog.RTN, RTN )
            , ( Prog.JOIN, JOIN )
            , ( Prog.AP, LoneAP )
            , ( Prog.DUM, DUM )
            ]


transpilerLDF : Test
transpilerLDF =
    Test.describe "LDF conditions" <|
        List.map
            (\( name, ops, expected ) ->
                Test.test name <|
                    \_ ->
                        transpile ops
                            |> Result.map (List.map stripIndices)
                            |> Expect.equal (Ok expected)
            )
            [ ( "LDF followed by funcbody"
              , [ Prog.LDF, Prog.FUNCBODY "sub" [ Prog.LD ( 0, 1 ), Prog.LD ( 0, 0 ), Prog.FUNC Prog.SUB, Prog.RTN ] ]
              , [ LDFunc "sub" <| List.map Unindexed [ LD ( 0, 1 ), LD ( 0, 0 ), FUNC "-", RTN ] ]
                    |> List.map Unindexed
              )
            , ( "LDF with nested and rap"
              , [ Prog.LDF, Prog.NESTED [ Prog.LDC 1, Prog.LD ( 0, 0 ), Prog.FUNC Prog.ADD, Prog.RTN ], Prog.RAP ]
              , [ LDApply RAP (List.map Unindexed [ LDC 1, LD ( 0, 0 ), FUNC "+", RTN ]) ]
                    |> List.map Unindexed
              )
            , ( "LDF during building args (no rap/ap)"
              , [ Prog.NIL, Prog.LDF, Prog.NESTED [ Prog.LDC 1, Prog.LD ( 0, 0 ), Prog.FUNC Prog.ADD, Prog.RTN ], Prog.FUNC Prog.CONS ]
              , [ NIL, LDLambda (List.map Unindexed [ LDC 1, LD ( 0, 0 ), FUNC "+", RTN ]), FUNC "CONS" ]
                    |> List.map Unindexed
              )
            , ( "((lambda (f) (f 1)) (lambda (x) (+ x 1)))"
              , [ Prog.NIL, Prog.LDF, Prog.NESTED [ Prog.LDC 1, Prog.LD ( 0, 0 ), Prog.FUNC Prog.ADD, Prog.RTN ], Prog.FUNC Prog.CONS, Prog.LDF, Prog.NESTED [ Prog.NIL, Prog.LDC 1, Prog.FUNC Prog.CONS, Prog.LD ( 0, 0 ), Prog.AP, Prog.RTN ], Prog.AP ]
              , [ NIL, LDLambda (List.map Unindexed [ LDC 1, LD ( 0, 0 ), FUNC "+", RTN ]), FUNC "CONS", LDApply AP (List.map Unindexed [ NIL, LDC 1, FUNC "CONS", LD ( 0, 0 ), LoneAP, RTN ]) ]
                    |> List.map Unindexed
              )
            ]


testTranspilerIndices : Test
testTranspilerIndices =
    Test.describe "Test transpiler for generating correct indices - assuming parser and compiler works" <|
        List.map
            (\( name, code ) ->
                let
                    indices =
                        AST.parse code
                            |> Result.mapError Util.deadEndsToString
                            |> Result.andThen Prog.compile
                            |> Result.andThen (Prog.toList >> transpile)
                            |> Result.map getIndices
                in
                Test.test name <|
                    \_ ->
                        case indices of
                            Ok ids ->
                                -- ensure ids are all unique
                                List.Extra.unique ids
                                    |> Expect.equal ids

                            Err err ->
                                Expect.fail <| "Transpiler index failed somewhere: " ++ err
            )
            [ ( "simple", "((lambda (x) (+ x 1)) 1)" )
            , ( "Mut Rec 4", Programs.mutuallyRecursiveIsEven 4 )
            , ( "Recursive Length", Programs.recursiveLength )
            , ( "Factorial 5", Programs.factorial 5 )
            , ( "fib 10", Programs.fibonacci 10 )
            ]
