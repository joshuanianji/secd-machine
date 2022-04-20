module Lib.LispASTSpec exposing (suite)

import Expect
import Fuzz
import Lib.Cons as Cons exposing (Cons)
import Lib.LispAST exposing (..)
import Parser exposing (Parser)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "LispAST Spec"
        [ parseExpectr ]


parseExpectr : Test
parseExpectr =
    Test.describe "LispAST.parser"
        [ testBasics
        , testFunctionApp
        , testIf
        , testTokens
        , testLambda
        , testLet
        , testQuote
        ]


testBasics : Test
testBasics =
    Test.describe "Basics"
        [ Test.fuzz Fuzz.int "ints" <|
            \x ->
                Expect.equal (Parser.run parseInt (String.fromInt x)) (Ok <| int x)
        , Test.test "Variable" <|
            \_ ->
                Expect.equal (Parser.run parseVar "x") (Ok <| var "x")
        ]


testFunctionApp : Test
testFunctionApp =
    Test.describe "Function application"
        [ binaryUnarySuccess
        , nArySuccess
        , complexApp
        ]


binaryUnarySuccess : Test
binaryUnarySuccess =
    Test.describe "Binary and Unary Functions succeed on" <|
        List.map (\( input, expected ) -> parseExpect parseFunctionApp input (Ok expected))
            [ ( "x", FuncApp (var "x") [] )
            , ( "atom 4", FuncApp (var "atom") [ int 4 ] )
            , ( "atom nil", FuncApp (var "atom") [ nil ] )
            , ( "null nil", FuncApp (var "null") [ nil ] )
            , ( "null x", FuncApp (var "null") [ var "x" ] )
            , ( "cdr nil", FuncApp (var "cdr") [ nil ] )
            , ( "car nil", FuncApp (var "car") [ nil ] )
            , ( "+ 1 2", FuncApp (var "+") [ int 1, int 2 ] )
            , ( "- 1 2", FuncApp (var "-") [ int 1, int 2 ] )
            , ( "* 1 2", FuncApp (var "*") [ int 1, int 2 ] )
            , ( "cons 1 nil", FuncApp (var "cons") [ int 1, nil ] )
            , ( "< 1 2", FuncApp (var "<") [ int 1, int 2 ] )
            , ( "> 1 2", FuncApp (var ">") [ int 1, int 2 ] )
            , ( "<= 1 2", FuncApp (var "<=") [ int 1, int 2 ] )
            , ( ">= 1 2", FuncApp (var ">=") [ int 1, int 2 ] )
            , ( "eq 1 2", FuncApp (var "eq") [ int 1, int 2 ] )
            , ( "* (+ 6 2) 3", FuncApp (var "*") [ FuncApp (var "+") [ int 6, int 2 ], int 3 ] )
            ]



-- in reality i wont allow a "+" function to take more than 2 args


nArySuccess : Test
nArySuccess =
    Test.describe "N-ary functions succeed on" <|
        List.map (\( input, expected ) -> parseExpect parseFunctionApp input (Ok expected))
            [ ( "list 1 2 3 4 5", FuncApp (var "list") [ int 1, int 2, int 3, int 4, int 5 ] )
            , ( "list 1 2 3 4 5 6 7 8 9 10", FuncApp (var "list") [ int 1, int 2, int 3, int 4, int 5, int 6, int 7, int 8, int 9, int 10 ] )
            , ( "+ 1 2 3 4 5", FuncApp (var "+") [ int 1, int 2, int 3, int 4, int 5 ] )
            ]



-- sometimes, the function we are applying arguments to can be an inline lambda function


complexApp : Test
complexApp =
    Test.describe "Complex Function Application" <|
        List.map (\( input, expected ) -> parseExpect parseFunctionApp input (Ok expected))
            [ ( "(lambda (x y) (+ x y)) 3 4"
              , FuncApp
                    (Lambda [ token "x", token "y" ] (FuncApp (var "+") [ var "x", var "y" ]))
                    [ int 3, int 4 ]
              )
            , ( "((lambda (x y) (+ x y)) 3) 4"
              , FuncApp
                    (FuncApp
                        (Lambda [ token "x", token "y" ] (FuncApp (var "+") [ var "x", var "y" ]))
                        [ int 3 ]
                    )
                    [ int 4 ]
              )
            , ( "len '(1 2 3 4 5)"
              , FuncApp (var "len") [ Quote <| Cons.fromList [ intVal 1, intVal 2, intVal 3, intVal 4, intVal 5 ] ]
              )
            ]


testIf : Test
testIf =
    Test.describe "If statements"
        [ parseExpect parseIf "if (eq 1 2) 3 4" (Ok <| If (FuncApp (var "eq") [ int 1, int 2 ]) (int 3) (int 4))
        , parseExpect parseIf "if (null x) y (cdr x)" (Ok <| If (FuncApp (var "null") [ var "x" ]) (var "y") (FuncApp (var "cdr") [ var "x" ]))
        , parseExpect parseIf "if  ( null   x )  y ( cdr x  ) " (Ok <| If (FuncApp (var "null") [ var "x" ]) (var "y") (FuncApp (var "cdr") [ var "x" ]))
        , parseExpect parseIf "if(null x)y(cdr x) " (Ok <| If (FuncApp (var "null") [ var "x" ]) (var "y") (FuncApp (var "cdr") [ var "x" ]))
        , parseExpect parseIf
            "if (null x) 0 (+ 1 (len (cdr x)))"
            (Ok <| If (FuncApp (var "null") [ var "x" ]) (int 0) (FuncApp (var "+") [ int 1, FuncApp (var "len") [ FuncApp (var "cdr") [ var "x" ] ] ]))
        ]



-- we use parseTokens to parse the arguments for lambda expressions


testTokens : Test
testTokens =
    Test.describe "A list of tokens"
        [ parseExpect (parseList parseToken) "()" (Ok <| [])
        , parseExpectErr (parseList parseToken) "(1 2 3)"
        , parseExpect (parseList parseToken) "(x y    z)" (Ok <| [ token "x", token "y", token "z" ])
        , parseExpect (parseList parseToken) "(x)" (Ok <| [ token "x" ])
        , parseExpect (parseList parseToken) "(  x )" (Ok <| [ token "x" ])
        ]


testLambda : Test
testLambda =
    Test.describe "Lambda functions" <|
        List.map (\( input, expected ) -> parseExpect parseLambda input (Ok expected))
            [ ( "lambda (x) x", Lambda [ token "x" ] (var "x") )
            , ( "lambda    (x)  x   ", Lambda [ token "x" ] (var "x") )
            , ( "lambda (x y) (+ x y)", Lambda [ token "x", token "y" ] (FuncApp (var "+") [ var "x", var "y" ]) )
            , ( "lambda  (x y z) x ", Lambda [ token "x", token "y", token "z" ] (var "x") )
            , ( "lambda () 3", Lambda [] (int 3) )
            ]



-- parse let and letrec


testLet : Test
testLet =
    Test.describe "Let and Letrec bindings"
        [ testLetSuccess
        , testLetRec
        ]


testLetSuccess : Test
testLetSuccess =
    Test.describe "Let statements that should succeed" <|
        List.map
            (\( input, expected ) -> parseExpect parseLet input (Ok expected))
            [ ( "let (x y) (1 2) (+ x y)"
              , Let
                    [ ( token "x", int 1 ), ( token "y", int 2 ) ]
                    (FuncApp (var "+") [ var "x", var "y" ])
              )
            , ( "let (x y )(1 2)   (+ x y )"
              , Let
                    [ ( token "x", int 1 ), ( token "y", int 2 ) ]
                    (FuncApp (var "+") [ var "x", var "y" ])
              )
            , ( "let (x) (((lambda (y) (+ y 1)) 2)) (* x x)"
              , Let
                    [ ( token "x", FuncApp (Lambda [ token "y" ] (FuncApp (var "+") [ var "y", int 1 ])) [ int 2 ] ) ]
                    (FuncApp (var "*") [ var "x", var "x" ])
              )
            ]


testLetRec : Test
testLetRec =
    Test.describe "Recursive functions using letrec"
        [ Test.test "Recursive length of a list" <|
            \_ ->
                let
                    lambda =
                        "lambda (x) (if (null x) 0 (+ 1 (len (cdr x))))"

                    -- assume lambdas parse as expected
                    lambdaExpected =
                        Parser.run parseLambda lambda

                    lenApp =
                        "len '(1 2 3 4 5)"

                    -- assume lambdas parse as expected
                    lenAppExpected =
                        Parser.run parseFunctionApp lenApp

                    input =
                        "letrec (len) ((" ++ lambda ++ ")) (" ++ lenApp ++ ")"

                    expected =
                        Result.map2 (\lExp lAppExp -> Letrec [ ( token "len", lExp ) ] lAppExp) lambdaExpected lenAppExpected
                in
                Expect.all
                    [ Expect.ok
                    , Expect.equal expected
                    ]
                    (Parser.run parseLetrec input)
        , Test.test "Factorial" <|
            \_ ->
                let
                    factDef =
                        "lambda (n) (if (eq n 0) 1 (* n (fact (- n 1))))"

                    -- assume lambdas parse as expected
                    defExpected =
                        Parser.run parseLambda factDef

                    factApp =
                        "fact 10"

                    -- assume lambdas parse as expected
                    appExpected =
                        Parser.run parseFunctionApp factApp

                    input =
                        "letrec (fact) ((" ++ factDef ++ ")) (" ++ factApp ++ ")"

                    expected =
                        Result.map2 (\lExp lAppExp -> Letrec [ ( token "fact", lExp ) ] lAppExp) defExpected appExpected
                in
                Expect.all
                    [ Expect.ok
                    , Expect.equal expected
                    ]
                    (Parser.run parseLetrec input)
        , Test.test "Fibonacci" <|
            \_ ->
                let
                    fibDef =
                        "lambda (n) (if (eq n 0) 0 (if (eq n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))"

                    -- assume lambdas parse as expected
                    defExpected =
                        Parser.run parseLambda fibDef

                    input =
                        "letrec (fib) ((" ++ fibDef ++ ")) (fib 10)"

                    expected =
                        Result.map (\def -> Letrec [ ( token "fib", def ) ] (FuncApp (var "fib") [ int 10 ])) defExpected
                in
                Expect.all
                    [ Expect.ok
                    , Expect.equal expected
                    ]
                    (Parser.run parseLetrec input)
        ]


testQuote : Test
testQuote =
    Test.describe "Quote function"
        [ quoteBasics
        , quoteNested
        ]


quoteBasics : Test
quoteBasics =
    let
        consVal : String -> Cons Value
        consVal =
            Cons.single << strVal
    in
    Test.describe "Quoting basic structures" <|
        List.map (\( input, expected ) -> parseExpect parseQuote input (Ok expected))
            [ ( "'a", Quote (consVal "a") )
            , ( "'b", Quote (consVal "b") )
            , ( "'(a)", Quote (Cons.fromList [ strVal "a" ]) )
            , ( "'lambda", Quote (consVal "lambda") )
            , ( "'(a b c)", Quote (Cons.fromList [ strVal "a", strVal "b", strVal "c" ]) )
            ]


quoteNested : Test
quoteNested =
    let
        consToken : String -> Cons Value
        consToken =
            Cons.single << strVal
    in
    Test.describe "Quoting nested structures" <|
        List.map (\( input, expected ) -> parseExpect parseQuote input (Ok expected))
            [ ( "'(lambda (x y) (+ x y))"
              , Quote (Cons.fromConsList [ consToken "lambda", Cons.fromList [ strVal "x", strVal "y" ], Cons.fromList [ strVal "+", strVal "x", strVal "y" ] ])
              )
            ]



-- runs a string a tests it against the expected result


parseExpect : Parser data -> String -> Result (List Parser.DeadEnd) data -> Test
parseExpect parser str res =
    Test.test str <|
        \_ ->
            Expect.equal (Parser.run parser str) res


parseExpectErr : Parser data -> String -> Test
parseExpectErr parser str =
    Test.test str <|
        \_ ->
            Expect.err (Parser.run parser str)
