module Lib.LispAST exposing (..)

import Char
import Lib.Cons as Cons exposing (Cons)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Trailing(..))
import Set



-- Intermediary representation of Lisp AST


type AST
    = Lambda (List Token) AST
    | BinaryOp BinaryOp AST AST -- builtin binary op
    | UnaryOp UnaryOp AST -- builtin unary op
    | If AST AST AST -- if-then-else
    | Let (List ( Token, AST )) AST -- let
    | Letrec (List ( Token, AST )) AST -- letrec
    | Quote (Cons Token) -- quote function (can be nested)
    | Integer Int
    | Var Token


type Token
    = Token String


type BinaryOp
    = ADD
    | MULT
    | SUB
    | CONS
    | COMPARE Cmp


type Cmp
    = CMP_EQ
    | CMP_LT
    | CMP_GT
    | CMP_LEQ
    | CMP_GEQ


type UnaryOp
    = ATOM -- true if the element is an atom (integer or boolean), NIL otherwise
    | CAR
    | CDR
    | NULL -- Returns true on Nil


nil : AST
nil =
    Quote Cons.Nil


var : String -> AST
var =
    Token >> Var



---- PARSER ----


parse : String -> Result (List DeadEnd) AST
parse =
    Parser.run parser


parser : Parser AST
parser =
    Parser.oneOf
        [ parseBinaryOp, parseLambda, parseNil, parseInt, parseVar ]



-- (+ AST AST)


parseBinaryOp : Parser AST
parseBinaryOp =
    let
        parseSymbol : Parser BinaryOp
        parseSymbol =
            Parser.oneOf
                [ Parser.token "+" |> Parser.map (\_ -> ADD)
                , Parser.token "*" |> Parser.map (\_ -> MULT)
                , Parser.token "-" |> Parser.map (\_ -> SUB)
                , Parser.token "cons" |> Parser.map (\_ -> CONS)
                , Parser.token "<=" |> Parser.map (\_ -> COMPARE CMP_LEQ)
                , Parser.token ">=" |> Parser.map (\_ -> COMPARE CMP_GEQ)
                , Parser.token "<" |> Parser.map (\_ -> COMPARE CMP_LT)
                , Parser.token ">" |> Parser.map (\_ -> COMPARE CMP_GT)
                , Parser.token "eq" |> Parser.map (\_ -> COMPARE CMP_EQ)
                ]
    in
    Parser.succeed BinaryOp
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseSymbol
        |. spaces
        |= Parser.lazy (\_ -> parser)
        |. spaces
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        |. Parser.symbol ")"


parseToken : Parser Token
parseToken =
    Parser.variable
        { start = \_ -> True
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "lambda", "let" ]
        }
        |> Parser.map Token



-- e.g. lambda (x y) (+ x y)


parseLambda : Parser AST
parseLambda =
    let
        parseArgs =
            Parser.sequence
                { start = "("
                , separator = " "
                , end = ")"
                , spaces = spaces
                , item = parseToken
                , trailing = Forbidden -- No trailing characters allowed
                }
    in
    Parser.succeed Lambda
        |. Parser.symbol "("
        |. Parser.spaces
        |. Parser.symbol "lambda"
        |. Parser.spaces
        |= parseArgs
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        |. Parser.symbol ")"


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
