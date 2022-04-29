module SECD.SECDSpec exposing (suite)

-- testing the ENTIRE SECD machine - from compile to execution

import Expect exposing (Expectation)
import Fuzz
import Lib.Cons as Cons
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util
import SECD.Program as Prog exposing (Program)
import SECD.VM as VM
import Test exposing (Test)


suite : Test
suite =
    Test.describe "SECD end-to-end test - is that what it's called?"
        [ basics
        , letStmts
        ]


basics : Test
basics =
    Test.describe "Basic e2e" <|
        List.map
            (\( code, expected ) ->
                Test.test
                    ("Expecting: '" ++ code ++ "' to equal " ++ VM.valueToString expected)
                <|
                    \_ -> expectCoderunSuccess code expected
            )
            [ ( "(* (+ 1 2) (+ 3 4))", VM.Integer 21 )
            , ( "(<= 1 2)", VM.Truthy )
            , ( "(<= 2 1)", VM.nil )
            , ( "(let ((x 1)) (+ x 2))", VM.Integer 3 )
            , ( "(if (> 1 2) 1 2)", VM.Integer 2 )
            ]


letStmts : Test
letStmts =
    Test.describe "Let and Letrec statement e2es" <|
        List.map
            (\( description, code, expected ) ->
                Test.test
                    (description ++ "\nExpecting: '" ++ code ++ "' to equal " ++ VM.valueToString expected)
                <|
                    \_ -> expectCoderunSuccess code expected
            )
            [ ( "Let - Nested let statements", "(let\n \t((x 1))\n \t(let \n \t((y 2))\n \t(let ((z 3))\n (* z (+ x y)))))", VM.Integer 9 )
            , ( "Let - Nested lambdas in let", "(let\n \t((x 1))\n \t(let \n \t((y 2))\n \t(let ((add1 (lambda (z) (+ z 1))))\n (add1 (+ x y)))))", VM.Integer 4 )
            , ( "Letrec - mutual recursion", "(letrec\n\t((odd \t(lambda (n) (if (eq n 0) nil (even (- n 1)))))\n\t (even \t(lambda (n) (if (eq n 0) t\t (odd (- n 1)))))) \n \t(even 4))", VM.Truthy )
            , ( "Let - manual currying", "(let\n \t((curriedAdd (lambda (x) (lambda (y) (+ x y)))))\n \t((curriedAdd 5) 10))", VM.Integer 15 )
            , ( "Letrec - sumlist", "(letrec \n \t((sumlist (lambda (l) (if (null l) 0 (+ (car l) (sumlist (cdr l)))))))\n\t(sumlist '(1 2 3)) \n)", VM.Integer 6 )
            , ( "letrec - maplist", "(letrec \n \t((maplist (lambda (f l) (if (null l) nil (cons (f (car l)) (maplist f (cdr l))))))\n (add1 (lambda (x) (+ x 1))))\n\t(maplist add1 '(1 2 3)) \n)", VM.Array <| Cons.fromList [ VM.Integer 2, VM.Integer 3, VM.Integer 4 ] )
            ]



-- runs a piece of code, and expect its execution to be the same as the expected value


expectCoderunSuccess : String -> VM.Value -> Expectation
expectCoderunSuccess code expected =
    AST.parse code
        |> Result.mapError Util.deadEndsToString
        |> Result.andThen Prog.compile
        |> Result.andThen (VM.initRaw >> VM.evaluate)
        |> Expect.equal (Ok expected)
