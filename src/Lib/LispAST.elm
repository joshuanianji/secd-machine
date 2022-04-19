module Lib.LispAST exposing (..)

import Char
import Lib.Cons as Cons exposing (Cons)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import Set



-- Intermediary representation of Lisp AST


type AST
    = Lambda (List Token) AST -- parameters and the body
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
    Parser.oneOf
        [ parseBinaryOp, parseUnaryOp, parseLambda, parseNil, parseInt, parseVar ]



-- (op AST AST)


parseBinaryOp : Parser AST
parseBinaryOp =
    let
        parseOp : Parser BinaryOp
        parseOp =
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
        |= parseOp
        |. spaces
        |= Parser.lazy (\_ -> parser)
        |. spaces
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        |. Parser.symbol ")"


parseUnaryOp : Parser AST
parseUnaryOp =
    let
        parseOp : Parser UnaryOp
        parseOp =
            Parser.oneOf
                [ Parser.token "atom" |> Parser.map (\_ -> ATOM)
                , Parser.token "car" |> Parser.map (\_ -> CAR)
                , Parser.token "cdr" |> Parser.map (\_ -> CDR)
                , Parser.token "null" |> Parser.map (\_ -> NULL)
                ]
    in
    Parser.succeed UnaryOp
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseOp
        |. spaces
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        |. Parser.symbol ")"


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
        |. Parser.symbol "("
        |. Parser.spaces
        |. Parser.symbol "lambda"
        |. spaces
        |= parseTokens
        |. spaces
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
