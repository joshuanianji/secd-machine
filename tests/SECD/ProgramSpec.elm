module SECD.ProgramSpec exposing (suite)

import Expect
import Fuzz
import Lib.Cons as Cons
import Lib.LispAST as AST
import SECD.Program exposing (..)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Program"
        [ testFromAST ]


testFromAST : Test
testFromAST =
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
        , Test.test "[2,3]" <|
            \_ ->
                compileArgs [ AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS ])
        , Test.test "[1,2,3]" <|
            \_ ->
                compileArgs [ AST.int 1, AST.int 2, AST.int 3 ]
                    |> Expect.equal (Ok <| [ NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS ])
        ]
