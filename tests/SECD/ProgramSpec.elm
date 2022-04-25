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
        [ testCompiler
        , testCompilerEnvironment
        ]


testCompiler : Test
testCompiler =
    Test.describe "Program.fromAST and auxiliary functions"
        [ integrationTests
        , unitTests
        ]


integrationTests : Test
integrationTests =
    Test.describe "Testing the entire Program.astToOps"
        [ integrationBasics
        , integrationIf
        ]


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


integrationIf : Test
integrationIf =
    Test.describe "If statement structures" <|
        [ Test.test "(if (ATOM 1) 1 2" <|
            \_ ->
                (compile_ emptyEnv <| AST.If (AST.FuncApp (AST.var "atom") [ AST.int 1 ]) (AST.int 1) (AST.int 2))
                    |> Expect.equal (Ok [ LDC 1, FUNC ATOM, SEL, NESTED [ LDC 1, JOIN ], NESTED [ LDC 2, JOIN ] ])
        ]


unitTests : Test
unitTests =
    Test.describe "Testing auxiliary functions separately"
        [ testCompileCons
        , testCompileArgsBuiltin
        , testCompileArgsNonbuiltin
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


testCompileArgsBuiltin : Test
testCompileArgsBuiltin =
    Test.describe "Program.compileArgs for builtin functions"
        [ Test.test "Empty args" <|
            \_ ->
                compileArgs emptyEnv True []
                    |> Expect.equal (Ok <| [])
        , Test.test "Singleton" <|
            \_ ->
                compileArgs emptyEnv True [ AST.int 1 ]
                    |> Expect.equal (Ok <| [ LDC 1 ])
        , Test.test "[2,3]" <|
            \_ ->
                compileArgs emptyEnv True [ AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ LDC 3, LDC 2 ])
        , Test.test "[1,2,3]" <|
            \_ ->
                compileArgs emptyEnv True [ AST.int 1, AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ LDC 3, LDC 2, LDC 1 ])
        ]


testCompileArgsNonbuiltin : Test
testCompileArgsNonbuiltin =
    Test.describe "Program.compileArgs for user functions"
        [ Test.test "Empty args" <|
            \_ ->
                compileArgs emptyEnv False []
                    |> Expect.equal (Ok <| [])
        , Test.test "Singleton" <|
            \_ ->
                compileArgs emptyEnv False [ AST.int 1 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 1, FUNC CONS ])
        , Test.test "[2,3]" <|
            \_ ->
                compileArgs emptyEnv False [ AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS ])
        , Test.test "[1,2,3]" <|
            \_ ->
                compileArgs emptyEnv False [ AST.int 1, AST.int 2, AST.int 3 ]
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
                        compileFunc emptyEnv (AST.var token)
                            |> Expect.equal (Ok ( Just expectedArgs, [ expectedFunc ], True ))
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
                        case compileFunc emptyEnv ast of
                            Ok ( Just _, _, _ ) ->
                                Expect.fail "This should fail!"

                            Ok ( Nothing, _, _ ) ->
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
                compileFunc emptyEnv (FuncApp (AST.var "+") [ AST.int 1 ])
                    |> Expect.equal (Ok ( Just 1, [ LDC 1, FUNC ADD ], True ))
        , Test.test "(+ 1 2)" <|
            \_ ->
                compileFunc emptyEnv (FuncApp (AST.var "+") [ AST.int 1, AST.int 2 ])
                    |> Expect.equal (Ok ( Just 0, [ LDC 2, LDC 1, FUNC ADD ], True ))
        , Test.test "Fails for too many arguments - ((+ 1) 2 3)" <|
            \_ ->
                compileFunc emptyEnv (FuncApp (FuncApp (AST.var "+") [ AST.int 1 ]) [ AST.int 2, AST.int 3 ])
                    |> Expect.err
        ]



---- COMPILER ENVIRONMENT ----


testCompilerEnvironment : Test
testCompilerEnvironment =
    Test.describe "Testing Compiler Environment"
        [ compiledEnvLookup
        , testAddVarNames
        ]


compiledEnvLookup : Test
compiledEnvLookup =
    let
        testEnv =
            Env [ [ "x", "z" ], [ "w", "q", "asdkjasdkas" ] ]
    in
    Test.describe "Environment Lookup"
        [ Test.test "lookup function - correctly looks up function closure" <|
            \_ ->
                lookup "asdkjasdkas" testEnv
                    |> Expect.equal (Ok <| LD ( 2, 3 ))
        , Test.test "lookup function - correctly looks up function closure - part 2" <|
            \_ ->
                lookup "x" testEnv
                    |> Expect.equal (Ok <| LD ( 1, 1 ))
        , Test.test "lookup function - correctly fails for unknown variable" <|
            \_ ->
                lookup "pppppp" testEnv
                    |> Expect.err
        ]


testAddVarNames : Test
testAddVarNames =
    let
        testEnv =
            Env [ [ "x", "z" ], [ "w", "q", "asdkjasdkas" ] ]
    in
    Test.describe "Test addLetBindings"
        [ Test.test "Succeeds with adding a single let bind" <|
            \_ ->
                addVarNames [ "pppppp" ] testEnv
                    |> Expect.equal
                        (Env [ [ "pppppp" ], [ "x", "z" ], [ "w", "q", "asdkjasdkas" ] ])
        , Test.test "Able to query after adding a let bind" <|
            \_ ->
                addVarNames [ "pppppp", "x", "z" ] testEnv
                    |> lookup "pppppp"
                    |> Expect.equal (Ok <| LD ( 1, 1 ))
        , Test.test "Query an earlier element after a let bind" <|
            \_ ->
                addVarNames [ "pppppp" ] testEnv
                    |> lookup "w"
                    |> Expect.equal (Ok <| LD ( 3, 1 ))
        , Test.test "More recent pushes to the env are treated with higher priority" <|
            \_ ->
                addVarNames [ "pppppp", "x" ] testEnv
                    |> lookup "x"
                    |> Expect.equal (Ok <| LD ( 1, 2 ))
        ]
