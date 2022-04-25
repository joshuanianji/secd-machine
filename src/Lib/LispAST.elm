module Lib.LispAST exposing (..)

import Char
import Lib.Cons as Cons exposing (Cons)
import List.Extra
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import Set



-- Intermediary representation of Lisp AST


type AST
    = Lambda (List Token) AST -- parameters and the body
    | FuncApp AST (List AST) -- function applied to n arguments. Function itself can be an AST (e.g. lambda), but we validate that AST separately
    | If AST AST AST -- if-then-else
    | Let (List ( Token, AST )) AST -- let
    | Letrec (List ( Token, AST )) AST -- letrec
    | Quote (Cons Int) -- quote function (can be nested)
    | Val Int -- an integer constant
    | Var Token -- variable



-- a variable or function


type Token
    = Token String


tokenToStr : Token -> String
tokenToStr (Token str) =
    str



-- constructors
-- mainly for writing tests


nil : AST
nil =
    Quote Cons.Nil



-- creates a variable


var : String -> AST
var =
    Token >> Var



-- creates an Integer constant


int : Int -> AST
int =
    Val



-- creates a token (variable or function)


token : String -> Token
token =
    Token



---- PARSER ----


parse : String -> Result (List DeadEnd) AST
parse =
    Parser.run <|
        Parser.succeed identity
            |= parser
            -- when parsing, ensure we parse the entire string
            |. Parser.end


parser : Parser AST
parser =
    let
        parseInParens =
            Parser.oneOf
                [ parseIf
                , parseFunctionApp
                , parseLambda
                , parseLet
                , parseLetrec
                , Parser.succeed <| Quote Cons.nil -- an empty parentheses is just treated as NIL
                ]

        parseWithoutParens =
            Parser.oneOf
                [ parseNil
                , parseValue
                , parseVariable
                , parseQuote
                ]
    in
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "("
            |. Parser.spaces
            |= parseInParens
            |. Parser.spaces
            |. Parser.symbol ")"
        , parseWithoutParens
        ]



-- (op AST AST)


parseFunctionApp : Parser AST
parseFunctionApp =
    let
        parseArgs : Parser (List AST)
        parseArgs =
            Parser.succeed identity
                |. Parser.spaces
                |= Parser.loop [] argsHelp

        argsHelp : List AST -> Parser (Step (List AST) (List AST))
        argsHelp revStmts =
            Parser.oneOf
                [ Parser.succeed (\stmt -> Loop (stmt :: revStmts))
                    |= Parser.lazy (\_ -> parser)
                    |. Parser.spaces
                , Parser.succeed ()
                    |> Parser.map (\_ -> Done (List.reverse revStmts))
                ]

        parseBuiltins : Parser AST
        parseBuiltins =
            Parser.oneOf
                [ Parser.map (\_ -> "+") <| Parser.token "+"
                , Parser.map (\_ -> "-") <| Parser.token "-"
                , Parser.map (\_ -> "*") <| Parser.token "*"
                , Parser.map (\_ -> "<=") <| Parser.token "<="
                , Parser.map (\_ -> ">=") <| Parser.token ">="
                , Parser.map (\_ -> "<") <| Parser.token "<"
                , Parser.map (\_ -> ">") <| Parser.token ">"
                ]
                |> Parser.map var
    in
    Parser.succeed FuncApp
        -- the function we apply can be a variable, a reserved function or an AST
        |= Parser.oneOf
            [ parseBuiltins
            , Parser.map Var parseToken
            , Parser.lazy (\_ -> parser)
            ]
        |. Parser.spaces
        |= parseArgs


parseIf : Parser AST
parseIf =
    Parser.succeed If
        |. Parser.symbol "if"
        |. Parser.spaces
        -- condition
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        -- true branch
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        -- false branch
        |= Parser.lazy (\_ -> parser)


parseVariable : Parser AST
parseVariable =
    Parser.map Var parseToken


parseToken : Parser Token
parseToken =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "lambda", "let", "letrec" ]
        }
        |> Parser.map Token



-- e.g. lambda (x y) (+ x y)


parseLambda : Parser AST
parseLambda =
    Parser.succeed Lambda
        |. Parser.keyword "lambda"
        |. spaces
        |= parseList parseToken
        |. spaces
        |= Parser.lazy (\_ -> parser)


parseLet : Parser AST
parseLet =
    parseGeneralLet "let" Let


parseLetrec : Parser AST
parseLetrec =
    parseGeneralLet "letrec" Letrec


parseGeneralLet : String -> (List ( Token, AST ) -> AST -> AST) -> Parser AST
parseGeneralLet letKeyword toLet =
    let
        parseLetBindings : Parser (List ( Token, AST ))
        parseLetBindings =
            Parser.succeed (\a b -> ( a, b ))
                |. Parser.spaces
                |= parseList parseToken
                |. Parser.spaces
                |= parseList (Parser.lazy (\_ -> parser))
                |> Parser.andThen
                    (\( tokens, vals ) ->
                        if List.length tokens /= List.length vals then
                            Parser.problem <| "Unequal number of tokens and values in " ++ letKeyword ++ "!"

                        else
                            Parser.succeed (List.Extra.zip tokens vals)
                    )
    in
    Parser.succeed toLet
        |. Parser.keyword letKeyword
        |. spaces
        |= parseLetBindings
        |. spaces
        |= Parser.lazy (\_ -> parser)


parseQuote : Parser AST
parseQuote =
    Parser.succeed Quote
        |. Parser.token "'"
        |= quoteParseCons



-- parses one singular cons element
-- calls parseInParens when it encounters parentheses


quoteParseCons : Parser (Cons Int)
quoteParseCons =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.map Cons.single <| parseInt
            , Parser.succeed identity
                |. Parser.symbol "("
                |. Parser.spaces
                |= Parser.lazy (\_ -> quoteParseInParens)
                |. Parser.spaces
                |. Parser.symbol ")"
            ]



-- parses in parentheses
-- recursively calls parseCons


quoteParseInParens : Parser (Cons Int)
quoteParseInParens =
    Parser.succeed identity
        |= Parser.loop [] quoteInParensHelper
        |> Parser.map Cons.fromConsList


quoteInParensHelper : List (Cons Int) -> Parser (Step (List (Cons Int)) (List (Cons Int)))
quoteInParensHelper revStmts =
    Parser.oneOf
        [ Parser.succeed (\stmt -> Loop (stmt :: revStmts))
            |= quoteParseCons
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse revStmts))
        ]


parseValue : Parser AST
parseValue =
    Parser.map Val parseInt


parseInt : Parser Int
parseInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


parseNil : Parser AST
parseNil =
    Parser.succeed nil
        |. Parser.symbol "nil"
        |. Parser.spaces



---- UTIL ----
-- parse one or more spaces


spaces : Parser ()
spaces =
    Parser.succeed ()
        |. Parser.token " "
        |. Parser.spaces



-- parse a list of space separated elements in parentheses, in the form (x1 x2 x3)


parseList : Parser a -> Parser (List a)
parseList parseListItem =
    Parser.succeed identity
        |. Parser.token "("
        |. Parser.spaces
        |= Parser.loop [] (tokensHelp parseListItem)


tokensHelp : Parser a -> List a -> Parser (Step (List a) (List a))
tokensHelp parseListItem revStmts =
    Parser.oneOf
        [ Parser.succeed (\stmt -> Loop (stmt :: revStmts))
            |= parseListItem
            |. Parser.spaces
        , Parser.token ")"
            |> Parser.map (\_ -> Done (List.reverse revStmts))
        ]
