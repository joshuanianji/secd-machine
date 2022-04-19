module Lib.LispAST exposing (..)

import Char
import Lib.Cons as Cons exposing (Cons)
import List.Extra
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import Set



-- Intermediary representation of Lisp AST


type AST
    = Lambda (List Token) AST -- parameters and the body
    | FuncApp AST (List AST) -- function applied to n arguments. Function itself can be an AST (e.g. lambda), but we validate that AST later on
    | If AST AST AST -- if-then-else
    | Let (List ( Token, AST )) AST -- let
    | Letrec (List ( Token, AST )) AST -- letrec
    | Quote (Cons Token) -- quote function (can be nested)
    | Integer Int
    | Var Token


type Token
    = Token String



-- constructors
-- mainly for writing tests


nil : AST
nil =
    Quote Cons.Nil


var : String -> AST
var =
    Token >> Var


token : String -> Token
token =
    Token


int : Int -> AST
int =
    Integer



---- PARSER ----


parse : String -> Result (List DeadEnd) AST
parse =
    Parser.run parser


parser : Parser AST
parser =
    let
        parseInParens =
            Parser.oneOf
                [ parseFunctionApp
                , parseLambda
                , parseLet
                , parseLetrec
                ]

        parseWithoutParens =
            Parser.oneOf
                [ parseNil
                , parseInt
                , parseVar
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

        reservedFuncs : Parser AST
        reservedFuncs =
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
        |= Parser.oneOf
            [ reservedFuncs
            , parseVar
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


parseInt : Parser AST
parseInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]
        |> Parser.map Integer


parseVar : Parser AST
parseVar =
    Parser.map Var parseToken


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
