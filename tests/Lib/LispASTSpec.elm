module Lib.LispASTSpec exposing (suite)

import Expect
import Fuzz
import Lib.Cons as Cons
import Lib.LispAST exposing (..)
import Parser exposing (Parser)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "LispAST Spec"
        [ integrations
        , unitParseTests
        ]



---- INTEGRATION TESTS ----
-- testing the entire parser
-- this adds testing like, () -> nil and '() -> nil


integrations : Test
integrations =
    Test.describe "Integration testing"
        [ integrationBasics
        , integrationFuncApps
        , integrationEdgeCases
        , integrationGeneral
        , integrationSpacing
        ]



-- Testing basics - e.g. parsing values


integrationBasics : Test
integrationBasics =
    Test.describe "Basics - parsing values"
        [ Test.fuzz Fuzz.int "Parses a number" <|
            \n ->
                Expect.equal (parse (String.fromInt n)) (Ok <| int n)
        , Test.test "Parses variable name 'A'" <|
            \_ ->
                Expect.equal (parse "A") (Ok <| var "A")
        , Test.test "Parses variable name 'A_B'" <|
            \_ ->
                Expect.equal (parse "A_B") (Ok <| var "A_B")
        , Test.test "Fails on variable name 'A-B'" <|
            \_ ->
                Expect.err (parse "A-B")
        , Test.test "Correctly parses a truthy value (does not parse it as a variable)" <|
            \_ ->
                Expect.equal (parse "t") (Ok Truthy)
        ]



-- function application, with some other stuff


integrationFuncApps : Test
integrationFuncApps =
    Test.describe "Function Application"
        [ Test.test "(+ a b) " <|
            \_ ->
                Expect.equal (parse "(+ a b)") (Ok <| FuncApp (var "+") [ var "a", var "b" ])
        , Test.test "(+ a b c) " <|
            \_ ->
                Expect.equal (parse "(+ a b c)") (Ok <| FuncApp (var "+") [ var "a", var "b", var "c" ])
        , Test.test "(fib 10)" <|
            \_ ->
                Expect.equal (parse "(fib 10)") (Ok <| FuncApp (var "fib") [ int 10 ])
        , Test.test "(fib (+ 3 4))" <|
            \_ ->
                Expect.equal (parse "(fib (+ 3 4))") (Ok <| FuncApp (var "fib") [ FuncApp (var "+") [ int 3, int 4 ] ])
        ]



-- me trying to think of edge cases that aren't already covered in the unit tests


integrationEdgeCases : Test
integrationEdgeCases =
    Test.describe "Edge Cases"
        [ Test.test "Parses empty quote list to nil " <|
            \_ ->
                Expect.equal (parse "'()") (Ok <| Quote Cons.nil)
        , Test.test "Parses empty list to nil" <|
            \_ ->
                Expect.equal (parse "()") (Ok <| Quote Cons.nil)
        , Test.test "(eq '() ())" <|
            \_ ->
                Expect.equal (parse "(eq () '())") (Ok <| FuncApp (var "eq") [ Quote Cons.nil, Quote Cons.nil ])
        , Test.test "Parses (x) as a function application" <|
            \_ ->
                Expect.equal (parse "(x)") (Ok <| FuncApp (var "x") [])
        , Test.test "Parses (+) as a function application (this is not valid once we compile it though)" <|
            \_ ->
                Expect.equal (parse "(+)") (Ok <| FuncApp (var "+") [])
        , Test.test "Function in let binding" <|
            \_ ->
                parse "(let ((f (lambda (x) (+ x 1)))) (f 3))"
                    |> Expect.equal
                        (Ok <|
                            Let [ ( token "f", Lambda [ token "x" ] (FuncApp (var "+") [ var "x", int 1 ]) ) ]
                                (FuncApp (var "f") [ int 3 ])
                        )
        ]


integrationGeneral : Test
integrationGeneral =
    Test.describe "Parsing bigger programs"
        [ Test.test "Recursive length program" <|
            \_ ->
                let
                    program =
                        "(letrec ((f (lambda (x m) (if (null x) m (f (cdr x) (+ m 1)))))) (f '(1 2 3) 0))"

                    expected =
                        Letrec [ ( token "f", Lambda [ token "x", token "m" ] (If (FuncApp (var "null") [ var "x" ]) (var "m") (FuncApp (var "f") [ FuncApp (var "cdr") [ var "x" ], FuncApp (var "+") [ var "m", Val 1 ] ])) ) ] (FuncApp (var "f") [ Quote <| Cons.fromList [ 1, 2, 3 ], Val 0 ])
                in
                parse program
                    |> Expect.equal (Ok expected)
        , Test.test "Mutually recursive isEven" <|
            \_ ->
                let
                    program =
                        "(letrec ((odd (lambda (n) (if (eq n 0) nil (even (- n 1))))) (even (lambda (n) (if (eq n 0) (atom nil) (odd (- n 1)))))) (even 4))"

                    oddFunc =
                        Lambda [ Token "n" ] (If (FuncApp (var "eq") [ var "n", Val 0 ]) nil (FuncApp (var "even") [ FuncApp (var "-") [ var "n", Val 1 ] ]))

                    evenFunc =
                        Lambda [ Token "n" ] (If (FuncApp (var "eq") [ var "n", Val 0 ]) (FuncApp (var "atom") [ nil ]) (FuncApp (var "odd") [ FuncApp (var "-") [ var "n", Val 1 ] ]))

                    expected =
                        Letrec [ ( Token "odd", oddFunc ), ( Token "even", evenFunc ) ] (FuncApp (var "even") [ Val 4 ])
                in
                parse program
                    |> Expect.equal (Ok expected)
        ]


integrationSpacing : Test
integrationSpacing =
    Test.describe "Parsing programs with lots of weird spacing and comments"
        [ Test.test "Recursive length program" <|
            \_ ->
                let
                    program =
                        "(letrec ((f (lambda (x m) (if (null x) m (f (cdr x) (+ m 1))))))(f '(1 2 3) 0))"

                    programWithSpacing =
                        "(letrec\n\n((f    (\t\tlambda (x m) (if (\n\nnull \t\t\tx) m (f(\t\tcdr x)(+ m 1)))\t\t)))(f'(1 2 3)0)\t\t)"
                in
                parse program
                    |> Expect.equal (parse programWithSpacing)
        , Test.test "Comments" <|
            \_ ->
                let
                    program =
                        ";comment without a space in front\n(let; comment without a space in front\n \t((curriedAdd (lambda (x) (lambda (y) (+ x y)))))\t ; comment with a space and tab\n \t((curriedAdd 5) 10)) ;ree"

                    programWithoutComments =
                        "(let\n \t((curriedAdd (lambda (x) (lambda (y) (+ x y)))))\t\n \t((curriedAdd 5) 10))"
                in
                parse program
                    |> Expect.equal (parse programWithoutComments)
        ]



---- UNIT TESTS ----
-- testing the parsers for each individual AST node


unitParseTests : Test
unitParseTests =
    Test.describe "Testing individual parsers"
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
                Expect.equal (Parser.run parseInt (String.fromInt x)) (Ok x)
        , Test.test "Variable" <|
            \_ ->
                Expect.equal (Parser.run parseToken "x") (Ok <| token "x")
        , Test.test "Truthy - parser success" <|
            \_ ->
                Expect.equal (Parser.run parseTruthy "t") (Ok Truthy)
        , Test.test "Truthy - parser failure" <|
            \_ ->
                Expect.err (Parser.run parseTruthy "f")
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
              , FuncApp (var "len") [ Quote <| Cons.fromList [ 1, 2, 3, 4, 5 ] ]
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
            [ ( "let ((x 1) (y 2)) (+ x y)"
              , Let
                    [ ( token "x", int 1 ), ( token "y", int 2 ) ]
                    (FuncApp (var "+") [ var "x", var "y" ])
              )
            , ( "let (  (x 1)(  y 2  ))   (+ x y )"
              , Let
                    [ ( token "x", int 1 ), ( token "y", int 2 ) ]
                    (FuncApp (var "+") [ var "x", var "y" ])
              )
            , ( "let ((x ((lambda (y) (+ y 1)) 2))) (* x x)"
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
                        "letrec ((len (" ++ lambda ++ "))) (" ++ lenApp ++ ")"

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

                    -- assume function application parse as expected
                    appExpected =
                        Parser.run parseFunctionApp factApp

                    input =
                        "letrec ((fact (" ++ factDef ++ "))) (" ++ factApp ++ ")"

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
                        "letrec ((fib (" ++ fibDef ++ "))) (fib 10)"

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
    Test.describe "Quote function - only accept nested integers"
        [ quoteBasics
        , quoteNested
        ]


quoteBasics : Test
quoteBasics =
    Test.describe "Quoting basic structures (lists)" <|
        List.map (\( input, expected ) -> parseExpect parseQuote input (Ok expected))
            [ ( "'()", Quote Cons.Nil )
            , ( "'(1)", Quote <| Cons.fromList [ 1 ] )
            , ( "'(1 2)", Quote <| Cons.fromList [ 1, 2 ] )
            , ( "'(1 2 3)", Quote <| Cons.fromList [ 1, 2, 3 ] )
            ]


quoteNested : Test
quoteNested =
    Test.describe "Quoting nested structures" <|
        List.map (\( input, expected ) -> parseExpect parseQuote input (Ok expected))
            [ ( "'(1 (2 3))", Quote <| Cons.fromConsList [ Cons.single 1, Cons.fromList [ 2, 3 ] ] )
            , ( "'(1 (2 3) 4)", Quote <| Cons.fromConsList [ Cons.single 1, Cons.fromList [ 2, 3 ], Cons.single 4 ] )
            , ( "'(((1 2) 3) 4)", Quote <| Cons.fromConsList [ Cons.fromConsList [ Cons.fromList [ 1, 2 ], Cons.single 3 ], Cons.single 4 ] )
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
