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

        -- "stress testing" environment handling
        -- , integrationEnv
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
        , testCompileFuncApp

        -- note that lambda and let are compiled extremely siimlarly
        , testCompileLet
        , testCompileLambda
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
                compileArgs emptyEnv BuiltinFunc []
                    |> Expect.equal (Ok <| [])
        , Test.test "Singleton" <|
            \_ ->
                compileArgs emptyEnv BuiltinFunc [ AST.int 1 ]
                    |> Expect.equal (Ok <| [ LDC 1 ])
        , Test.test "[2,3]" <|
            \_ ->
                compileArgs emptyEnv BuiltinFunc [ AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ LDC 3, LDC 2 ])
        , Test.test "[1,2,3]" <|
            \_ ->
                compileArgs emptyEnv BuiltinFunc [ AST.int 1, AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ LDC 3, LDC 2, LDC 1 ])
        ]


testCompileArgsNonbuiltin : Test
testCompileArgsNonbuiltin =
    Test.describe "Program.compileArgs for user defined/lambda functions"
        [ Test.test "Empty args - Lambda" <|
            \_ ->
                compileArgs emptyEnv LambdaFunc []
                    |> Expect.equal (Ok <| [])
        , Test.test "Empty args - Loaded" <|
            \_ ->
                compileArgs emptyEnv LoadedFunc []
                    |> Expect.equal (Ok <| [])
        , Test.test "Singleton - Lambda" <|
            \_ ->
                compileArgs emptyEnv LambdaFunc [ AST.int 1 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 1, FUNC CONS ])
        , Test.test "Singleton - Loaded" <|
            \_ ->
                compileArgs emptyEnv LoadedFunc [ AST.int 1 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 1, FUNC CONS ])
        , Test.test "[2,3] - Lambda" <|
            \_ ->
                compileArgs emptyEnv LambdaFunc [ AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS ])
        , Test.test "[2,3] - Loaded" <|
            \_ ->
                compileArgs emptyEnv LoadedFunc [ AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS ])
        , Test.test "[1,2,3] - Lambda" <|
            \_ ->
                compileArgs emptyEnv LambdaFunc [ AST.int 1, AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS ])
        , Test.test "[1,2,3] - Loaded" <|
            \_ ->
                compileArgs emptyEnv LoadedFunc [ AST.int 1, AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS ])
        ]


testCompileFunc : Test
testCompileFunc =
    Test.describe "Program.compileFunc"
        [ testCompileFuncBuiltins
        , testCompileFuncIllegalCalls
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
                            |> Expect.equal (Ok ( Just expectedArgs, [ expectedFunc ], BuiltinFunc ))
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


testCompileFuncApp : Test
testCompileFuncApp =
    Test.describe "Program.testCompileFunc"
        [ Test.test "Builtin (-)" <|
            \_ ->
                compileFuncApp emptyEnv (AST.var "-") [ AST.int 1, AST.int 2 ]
                    |> Expect.equal (Ok [ LDC 2, LDC 1, FUNC SUB ])
        , Test.test "Builtin (CONS)" <|
            \_ ->
                compileFuncApp emptyEnv (AST.var "cons") [ AST.int 1, AST.nil ]
                    |> Expect.equal (Ok [ NIL, LDC 1, FUNC CONS ])
        , Test.test "Builtin - fails for too many args 2 for ATOM)" <|
            \_ ->
                compileFuncApp emptyEnv (AST.var "atom") [ AST.int 1, AST.nil ]
                    |> Expect.err
        , Test.test "Lambda fails with not enough args - ((lambda (x y) (+ x y)) 2)" <|
            \_ ->
                compileFunc emptyEnv (FuncApp (Lambda [ AST.token "x", AST.token "y" ] (AST.FuncApp (AST.var "+") [ AST.var "x", AST.var "y" ])) [ AST.int 2 ])
                    |> Expect.err
        , Test.test "Evaluate arithmetic (- 1 2)" <|
            \_ ->
                compileFuncApp emptyEnv (AST.var "-") [ AST.int 1, AST.int 2 ]
                    |> Expect.equal (Ok [ LDC 2, LDC 1, FUNC SUB ])
        , Test.test "Slightly more complex arithmetic - (+ (- 1 2) 3)" <|
            \_ ->
                compileFuncApp emptyEnv (AST.var "+") [ AST.FuncApp (AST.var "-") [ AST.int 1, AST.int 2 ], AST.int 3 ]
                    |> Expect.equal (Ok [ LDC 3, LDC 2, LDC 1, FUNC SUB, FUNC ADD ])
        , Test.test "Deeper nested arithmetic - (+ (- 1 (* 2 5)) 3)" <|
            \_ ->
                compileFuncApp emptyEnv (AST.var "+") [ AST.FuncApp (AST.var "-") [ AST.int 1, AST.FuncApp (AST.var "*") [ AST.int 2, AST.int 5 ] ], AST.int 3 ]
                    |> Expect.equal (Ok [ LDC 3, LDC 5, LDC 2, FUNC MULT, LDC 1, FUNC SUB, FUNC ADD ])
        , Test.test "Deeper nested arithmetic fails for too many args - (+ (- 1 (* 2 5 3)) 3)" <|
            \_ ->
                compileFuncApp emptyEnv (AST.var "+") [ AST.FuncApp (AST.var "-") [ AST.int 1, AST.FuncApp (AST.var "*") [ AST.int 2, AST.int 5, AST.int 3 ] ], AST.int 3 ]
                    |> Expect.err
        ]


testCompileLet : Test
testCompileLet =
    Test.describe "Program.compileLet" <|
        [ Test.test "compiled (let (x) (1) (+ x 2))" <|
            \_ ->
                compileLet emptyEnv [ ( AST.token "x", AST.Val 1 ) ] (AST.FuncApp (AST.var "+") [ AST.var "x", AST.Val 2 ])
                    |> Expect.equal (Ok [ NIL, LDC 1, FUNC CONS, LDF, NESTED [ LDC 2, LD ( 1, 1 ), FUNC ADD, RTN ], AP ])
        , Test.test "lambda binding - (let (f) ((lambda (x) (+ x 1))) (f 3))" <|
            \_ ->
                compileLet emptyEnv
                    [ ( AST.token "f", AST.Lambda [ AST.token "x" ] (AST.FuncApp (AST.var "+") [ AST.var "x", AST.int 1 ]) ) ]
                    (AST.FuncApp (AST.var "f") [ AST.int 3 ])
                    |> Expect.equal (Ok [ NIL, LDF, NESTED [ LDC 1, LD ( 1, 1 ), FUNC ADD, RTN ], FUNC CONS, LDF, NESTED [ NIL, LDC 3, FUNC CONS, LD ( 1, 1 ), AP, RTN ], AP ])
        ]


testCompileLambda : Test
testCompileLambda =
    Test.describe "Program.compileLambda" <|
        [ Test.test "compiled (lambda (x y) (+ x y))" <|
            \_ ->
                compileLambda emptyEnv [ AST.token "x", AST.token "y" ] (AST.FuncApp (AST.var "+") [ AST.var "x", AST.var "y" ])
                    |> Expect.equal (Ok [ LDF, NESTED [ LD ( 1, 2 ), LD ( 1, 1 ), FUNC ADD, RTN ] ])
        , Test.test "Nested Lambda - (lambda (z) ((lambda (x y) (+ (- x y) z)) 3 5))" <|
            \_ ->
                compileLambda emptyEnv
                    [ AST.token "z" ]
                    (AST.FuncApp
                        (AST.Lambda [ AST.token "x", AST.token "y" ]
                            (AST.FuncApp (AST.var "+") [ AST.FuncApp (AST.var "-") [ AST.var "x", AST.var "y" ], AST.var "z" ])
                        )
                        [ AST.Val 3, AST.Val 5 ]
                    )
                    |> Expect.equal
                        (Ok [ LDF, NESTED [ NIL, LDC 5, FUNC CONS, LDC 3, FUNC CONS, LDF, NESTED [ LD ( 2, 1 ), LD ( 1, 2 ), LD ( 1, 1 ), FUNC SUB, FUNC ADD, RTN ], AP, RTN ] ])
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
