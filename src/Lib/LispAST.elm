module Lib.LispAST exposing (..)

import Char
import Lib.Cons as Cons exposing (Cons)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import Set



-- Intermediary representation of Lisp AST


type AST
    = Lambda (List Token) AST -- parameters and the body
    | FuncApp Token (List AST) -- function applied to n arguments
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

        reservedFuncs : Parser Token
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
                |> Parser.map token
    in
    Parser.succeed FuncApp
        |= Parser.oneOf [ reservedFuncs, parseToken ]
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
        |. Parser.symbol "lambda"
        |. spaces
        |= parseTokens
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



-- parse a list of tokens, for a lambda expression arguments


parseTokens : Parser (List Token)
parseTokens =
    Parser.succeed identity
        |. Parser.token "("
        |. Parser.spaces
        |= Parser.loop [] tokensHelp


tokensHelp : List Token -> Parser (Step (List Token) (List Token))
tokensHelp revStmts =
    Parser.oneOf
        [ Parser.succeed (\stmt -> Loop (stmt :: revStmts))
            |= parseToken
            |. Parser.spaces
        , Parser.token ")"
            |> Parser.map (\_ -> Done (List.reverse revStmts))
        ]
