module SECD.ProgramSpec exposing (suite)

import Expect
import Fuzz
import Lib.Cons as Cons
import Lib.LispAST as AST exposing (AST(..))
import SECD.Program exposing (..)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Program"
        [ testCompiler ]


testCompiler : Test
testCompiler =
    Test.describe "Program.fromAST and auxiliary functions"
        [ integrationTests
        , unitTests
        ]


integrationTests : Test
integrationTests =
    Test.describe "Testing the entire Program.astToOps"
        [ integrationBasics ]


integrationBasics : Test
integrationBasics =
    Test.describe "Variables and values"
        [ Test.fuzz Fuzz.int "Compiles an integer" <|
            \n ->
                let
                    ast =
                        AST.int n

                    expected =
                        Ok <| fromList [ LDC n ]
                in
                Expect.equal (compile ast) expected
        , Test.test "Compiles NIL variable" <|
            \_ ->
                Expect.equal (compile <| AST.var "nil") <| (Ok <| fromList [ NIL ])
        ]


unitTests : Test
unitTests =
    Test.describe "Testing auxiliary functions separately"
        [ testCompileCons
        , testCompileArgs
        , testCompileFunc
        ]


testCompileCons : Test
testCompileCons =
    Test.describe "Program.compileCons"
        [ Test.test "empty list" <|
            \_ ->
                Expect.equal (compileCons Cons.nil) [ NIL ]
        , Test.test "Singleton list" <|
            \_ ->
                Expect.equal (compileCons (Cons.fromList [ 1 ])) [ NIL, LDC 1, FUNC CONS ]
        , Test.test "(1 2 3)" <|
            \_ ->
                Expect.equal (compileCons (Cons.fromList [ 1, 2, 3 ])) [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS ]
        , Test.test "((1 2 3) 4)" <|
            \_ ->
                Expect.equal (compileCons (Cons.fromConsList [ Cons.fromList [ 1, 2, 3 ], Cons.single 4 ])) [ NIL, LDC 4, FUNC CONS, NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC CONS ]
        , Test.test "(((1 2)))" <|
            \_ ->
                Expect.equal (compileCons (Cons.fromConsList [ Cons.fromConsList [ Cons.fromList [ 1, 2 ] ] ])) [ NIL, NIL, NIL, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC CONS, FUNC CONS ]
        ]


testCompileArgs : Test
testCompileArgs =
    Test.describe "Program.compileArgs"
        [ Test.test "Empty args" <|
            \_ ->
                compileArgs []
                    |> Expect.equal (Ok <| [])
        , Test.test "Singleton" <|
            \_ ->
                compileArgs [ AST.int 1 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 1, FUNC CONS ])
        , Test.test "[2,3]" <|
            \_ ->
                compileArgs [ AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS ])
        , Test.test "[1,2,3]" <|
            \_ ->
                compileArgs [ AST.int 1, AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS ])
        ]


testCompileFunc : Test
testCompileFunc =
    Test.describe "Program.compileFunc"
        [ testCompileFuncBuiltins
        , testCompileFuncIllegalCalls
        , testCompileFuncCurrying
        ]



-- Testing that builtin functions work


testCompileFuncBuiltins : Test
testCompileFuncBuiltins =
    Test.describe "Program.compileFunc with builtin functions" <|
        List.map
            (\( token, expectedArgs, expectedFunc ) ->
                Test.test ("Compiles " ++ token) <|
                    \_ ->
                        compileFunc (AST.var token)
                            |> Expect.equal (Ok ( Just expectedArgs, [ expectedFunc ] ))
            )
            [ ( "+", 2, FUNC ADD )
            , ( "-", 2, FUNC SUB )
            , ( "*", 2, FUNC MULT )
            , ( "eq", 2, FUNC <| COMPARE CMP_EQ )
            , ( "<", 2, FUNC <| COMPARE CMP_LT )
            , ( ">", 2, FUNC <| COMPARE CMP_GT )
            , ( ">=", 2, FUNC <| COMPARE CMP_GEQ )
            , ( "<=", 2, FUNC <| COMPARE CMP_LEQ )
            , ( "cons", 2, FUNC CONS )
            , ( "car", 1, FUNC CAR )
            , ( "cdr", 1, FUNC CDR )
            , ( "atom", 1, FUNC ATOM )
            ]



-- Tests that our compiler correctly recognized illegal function calls


testCompileFuncIllegalCalls : Test
testCompileFuncIllegalCalls =
    Test.describe "Program.compileFunc with illegal function calls" <|
        List.map
            (\( name, ast ) ->
                Test.test ("Does not compile " ++ name) <|
                    \_ ->
                        case compileFunc ast of
                            Ok ( Just _, _ ) ->
                                Expect.fail "This should fail!"

                            Ok ( Nothing, _ ) ->
                                Expect.fail <| "compileFunc is acting weird with this ast: " ++ name ++ ". Not returning valid arity"

                            Err _ ->
                                Expect.pass
            )
            [ ( "'nil", Quote Cons.Nil )
            , ( "'(1 2 3)", Quote (Cons.fromList [ 1, 2, 3 ]) )
            ]


testCompileFuncCurrying : Test
testCompileFuncCurrying =
    Test.describe "Program.compileFunc with currying" <|
        [ Test.test "(+ 1)" <|
            \_ ->
                compileFunc (FuncApp (AST.var "+") [ AST.int 1 ])
                    |> Expect.equal (Ok ( Just 1, [ NIL, LDC 1, FUNC CONS, FUNC ADD ] ))
        , Test.test "(+ 1 2)" <|
            \_ ->
                compileFunc (FuncApp (AST.var "+") [ AST.int 1, AST.int 2 ])
                    |> Expect.equal (Ok ( Just 0, [ NIL, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC ADD ] ))
        ]
