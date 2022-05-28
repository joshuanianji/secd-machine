module Views.CompiledSpec exposing (suite)

import Expect
import SECD.Program as Prog
import Test exposing (Test)
import Views.Compiled exposing (APType(..), Code(..), transpile)


suite : Test
suite =
    Test.describe "compiledSpec"
        [ testTranspiler ]


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
                            |> Expect.equal (Ok [ expected ])
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
                            |> Expect.equal (Ok expected)
            )
            [ ( "LDF followed by funcbody"
              , [ Prog.LDF, Prog.FUNCBODY "sub" [ Prog.LD ( 0, 1 ), Prog.LD ( 0, 0 ), Prog.FUNC Prog.SUB, Prog.RTN ] ]
              , [ LDFunc "sub" ]
              )
            , ( "LDF with nested and rap"
              , [ Prog.LDF, Prog.NESTED [ Prog.LDC 1, Prog.LD ( 0, 0 ), Prog.FUNC Prog.ADD, Prog.RTN ], Prog.RAP ]
              , [ LDApply [ LDC 1, LD ( 0, 0 ), FUNC "+", RTN ] RAP ]
              )
            , ( "LDF during building args (no rap/ap)"
              , [ Prog.NIL, Prog.LDF, Prog.NESTED [ Prog.LDC 1, Prog.LD ( 0, 0 ), Prog.FUNC Prog.ADD, Prog.RTN ], Prog.FUNC Prog.CONS ]
              , [ NIL, LDLambda [ LDC 1, LD ( 0, 0 ), FUNC "+", RTN ], FUNC "CONS" ]
              )
            , ( "((lambda (f) (f 1)) (lambda (x) (+ x 1)))"
              , [ Prog.NIL, Prog.LDF, Prog.NESTED [ Prog.LDC 1, Prog.LD ( 0, 0 ), Prog.FUNC Prog.ADD, Prog.RTN ], Prog.FUNC Prog.CONS, Prog.LDF, Prog.NESTED [ Prog.NIL, Prog.LDC 1, Prog.FUNC Prog.CONS, Prog.LD ( 0, 0 ), Prog.AP, Prog.RTN ], Prog.AP ]
              , [ NIL, LDLambda [ LDC 1, LD ( 0, 0 ), FUNC "+", RTN ], FUNC "CONS", LDApply [ NIL, LDC 1, FUNC "CONS", LD ( 0, 0 ), LoneAP, RTN ] AP ]
              )
            ]
