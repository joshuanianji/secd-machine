module Gen.Lib.LispAST exposing (annotation_, call_, caseOf_, int, make_, moduleName_, nil, parse, parseComment, parseFunctionApp, parseGeneralLet, parseIf, parseInt, parseLambda, parseLet, parseLetrec, parseList, parseNil, parseNumeral, parseQuote, parseToken, parseTruthy, parseValue, parseVariable, parser, quoteInParensHelper, quoteParseCons, quoteParseInParens, spaces, token, tokenToStr, tokensHelp, values_, var)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, tokenToStr, nil, var, int, token, parse, parser, parseFunctionApp, parseIf, parseVariable, parseToken, parseTruthy, parseLambda, parseLet, parseLetrec, parseGeneralLet, parseQuote, quoteParseCons, quoteParseInParens, quoteInParensHelper, parseValue, parseInt, parseNumeral, parseNil, parseComment, spaces, parseList, tokensHelp, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Lib", "LispAST" ]


{-| tokensHelp: Parser a -> List a -> Parser (Step (List a) (List a)) -}
tokensHelp : Elm.Expression -> List Elm.Expression -> Elm.Expression
tokensHelp tokensHelpArg tokensHelpArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "tokensHelp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Parser" [ Type.var "a" ]
                        , Type.list (Type.var "a")
                        ]
                        (Type.namedWith
                            []
                            "Parser"
                            [ Type.namedWith
                                []
                                "Step"
                                [ Type.list (Type.var "a")
                                , Type.list (Type.var "a")
                                ]
                            ]
                        )
                    )
            }
        )
        [ tokensHelpArg, Elm.list tokensHelpArg0 ]


{-| parseList: Parser a -> Parser (List a) -}
parseList : Elm.Expression -> Elm.Expression
parseList parseListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Parser" [ Type.var "a" ] ]
                        (Type.namedWith [] "Parser" [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ parseListArg ]


{-| spaces: Parser () -}
spaces : Elm.Expression
spaces =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "spaces"
        , annotation = Just (Type.namedWith [] "Parser" [ Type.unit ])
        }


{-| parseComment: Parser () -}
parseComment : Elm.Expression
parseComment =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseComment"
        , annotation = Just (Type.namedWith [] "Parser" [ Type.unit ])
        }


{-| parseNil: Parser AST -}
parseNil : Elm.Expression
parseNil =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseNil"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseNumeral: Parser Int -}
parseNumeral : Elm.Expression
parseNumeral =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseNumeral"
        , annotation = Just (Type.namedWith [] "Parser" [ Type.int ])
        }


{-| parseInt: Parser Int -}
parseInt : Elm.Expression
parseInt =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseInt"
        , annotation = Just (Type.namedWith [] "Parser" [ Type.int ])
        }


{-| parseValue: Parser AST -}
parseValue : Elm.Expression
parseValue =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseValue"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| quoteInParensHelper: List (Cons Int) -> Parser (Step (List (Cons Int)) (List (Cons Int))) -}
quoteInParensHelper : List Elm.Expression -> Elm.Expression
quoteInParensHelper quoteInParensHelperArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "quoteInParensHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Cons" [ Type.int ]) ]
                        (Type.namedWith
                            []
                            "Parser"
                            [ Type.namedWith
                                []
                                "Step"
                                [ Type.list
                                    (Type.namedWith [] "Cons" [ Type.int ])
                                , Type.list
                                    (Type.namedWith [] "Cons" [ Type.int ])
                                ]
                            ]
                        )
                    )
            }
        )
        [ Elm.list quoteInParensHelperArg ]


{-| quoteParseInParens: Parser (Cons Int) -}
quoteParseInParens : Elm.Expression
quoteParseInParens =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "quoteParseInParens"
        , annotation =
            Just
                (Type.namedWith
                    []
                    "Parser"
                    [ Type.namedWith [] "Cons" [ Type.int ] ]
                )
        }


{-| quoteParseCons: Parser (Cons Int) -}
quoteParseCons : Elm.Expression
quoteParseCons =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "quoteParseCons"
        , annotation =
            Just
                (Type.namedWith
                    []
                    "Parser"
                    [ Type.namedWith [] "Cons" [ Type.int ] ]
                )
        }


{-| parseQuote: Parser AST -}
parseQuote : Elm.Expression
parseQuote =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseQuote"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseGeneralLet: String -> (List ( Token, AST ) -> AST -> AST) -> Parser AST -}
parseGeneralLet :
    String
    -> (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
parseGeneralLet parseGeneralLetArg parseGeneralLetArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseGeneralLet"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.list
                                (Type.tuple
                                    (Type.namedWith [] "Token" [])
                                    (Type.namedWith [] "AST" [])
                                )
                            , Type.namedWith [] "AST" []
                            ]
                            (Type.namedWith [] "AST" [])
                        ]
                        (Type.namedWith
                            []
                            "Parser"
                            [ Type.namedWith [] "AST" [] ]
                        )
                    )
            }
        )
        [ Elm.string parseGeneralLetArg
        , Elm.functionReduced
            "parseGeneralLetUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (parseGeneralLetArg0 functionReducedUnpack)
            )
        ]


{-| parseLetrec: Parser AST -}
parseLetrec : Elm.Expression
parseLetrec =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseLetrec"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseLet: Parser AST -}
parseLet : Elm.Expression
parseLet =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseLet"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseLambda: Parser AST -}
parseLambda : Elm.Expression
parseLambda =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseLambda"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseTruthy: Parser AST -}
parseTruthy : Elm.Expression
parseTruthy =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseTruthy"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseToken: Parser Token -}
parseToken : Elm.Expression
parseToken =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseToken"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "Token" [] ])
        }


{-| parseVariable: Parser AST -}
parseVariable : Elm.Expression
parseVariable =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseVariable"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseIf: Parser AST -}
parseIf : Elm.Expression
parseIf =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseIf"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parseFunctionApp: Parser AST -}
parseFunctionApp : Elm.Expression
parseFunctionApp =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parseFunctionApp"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parser: Parser AST -}
parser : Elm.Expression
parser =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "parser"
        , annotation =
            Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
        }


{-| parse: String -> Result (List DeadEnd) AST -}
parse : String -> Elm.Expression
parse parseArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parse"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.list (Type.namedWith [] "DeadEnd" [])
                            , Type.namedWith [] "AST" []
                            ]
                        )
                    )
            }
        )
        [ Elm.string parseArg ]


{-| token: String -> Token -}
token : String -> Elm.Expression
token tokenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "token"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Token" [])
                    )
            }
        )
        [ Elm.string tokenArg ]


{-| int: Int -> AST -}
int : Int -> Elm.Expression
int intArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "int"
            , annotation =
                Just (Type.function [ Type.int ] (Type.namedWith [] "AST" []))
            }
        )
        [ Elm.int intArg ]


{-| var: String -> AST -}
var : String -> Elm.Expression
var varArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "var"
            , annotation =
                Just
                    (Type.function [ Type.string ] (Type.namedWith [] "AST" []))
            }
        )
        [ Elm.string varArg ]


{-| nil: AST -}
nil : Elm.Expression
nil =
    Elm.value
        { importFrom = [ "Lib", "LispAST" ]
        , name = "nil"
        , annotation = Just (Type.namedWith [] "AST" [])
        }


{-| tokenToStr: Token -> String -}
tokenToStr : Elm.Expression -> Elm.Expression
tokenToStr tokenToStrArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "tokenToStr"
            , annotation =
                Just
                    (Type.function [ Type.namedWith [] "Token" [] ] Type.string)
            }
        )
        [ tokenToStrArg ]


annotation_ : { token : Type.Annotation, aST : Type.Annotation }
annotation_ =
    { token = Type.namedWith [ "Lib", "LispAST" ] "Token" []
    , aST = Type.namedWith [ "Lib", "LispAST" ] "AST" []
    }


make_ :
    { token : Elm.Expression -> Elm.Expression
    , lambda : Elm.Expression -> Elm.Expression -> Elm.Expression
    , funcApp : Elm.Expression -> Elm.Expression -> Elm.Expression
    , if_ : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , let_ : Elm.Expression -> Elm.Expression -> Elm.Expression
    , letrec : Elm.Expression -> Elm.Expression -> Elm.Expression
    , quote : Elm.Expression -> Elm.Expression
    , val : Elm.Expression -> Elm.Expression
    , var : Elm.Expression -> Elm.Expression
    , truthy : Elm.Expression
    }
make_ =
    { token =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "Token"
                    , annotation = Just (Type.namedWith [] "Token" [])
                    }
                )
                [ ar0 ]
    , lambda =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "Lambda"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0, ar1 ]
    , funcApp =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "FuncApp"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0, ar1 ]
    , if_ =
        \ar0 ar1 ar2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "If"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0, ar1, ar2 ]
    , let_ =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "Let"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0, ar1 ]
    , letrec =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "Letrec"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0, ar1 ]
    , quote =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "Quote"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0 ]
    , val =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "Val"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0 ]
    , var =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "Var"
                    , annotation = Just (Type.namedWith [] "AST" [])
                    }
                )
                [ ar0 ]
    , truthy =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "Truthy"
            , annotation = Just (Type.namedWith [] "AST" [])
            }
    }


caseOf_ :
    { token :
        Elm.Expression
        -> { tokenTags_0_0 | token : Elm.Expression -> Elm.Expression }
        -> Elm.Expression
    , aST :
        Elm.Expression
        -> { aSTTags_1_0
            | lambda : Elm.Expression -> Elm.Expression -> Elm.Expression
            , funcApp : Elm.Expression -> Elm.Expression -> Elm.Expression
            , if_ :
                Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
            , let_ : Elm.Expression -> Elm.Expression -> Elm.Expression
            , letrec : Elm.Expression -> Elm.Expression -> Elm.Expression
            , quote : Elm.Expression -> Elm.Expression
            , val : Elm.Expression -> Elm.Expression
            , var : Elm.Expression -> Elm.Expression
            , truthy : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { token =
        \tokenExpression tokenTags ->
            Elm.Case.custom
                tokenExpression
                (Type.namedWith [ "Lib", "LispAST" ] "Token" [])
                [ Elm.Case.branch1
                    "Token"
                    ( "string.String", Type.string )
                    tokenTags.token
                ]
    , aST =
        \aSTExpression aSTTags ->
            Elm.Case.custom
                aSTExpression
                (Type.namedWith [ "Lib", "LispAST" ] "AST" [])
                [ Elm.Case.branch2
                    "Lambda"
                    ( "list.List", Type.list (Type.namedWith [] "Token" []) )
                    ( "aST", Type.namedWith [] "AST" [] )
                    aSTTags.lambda
                , Elm.Case.branch2
                    "FuncApp"
                    ( "aST", Type.namedWith [] "AST" [] )
                    ( "list.List", Type.list (Type.namedWith [] "AST" []) )
                    aSTTags.funcApp
                , Elm.Case.branch3
                    "If"
                    ( "aST", Type.namedWith [] "AST" [] )
                    ( "aST", Type.namedWith [] "AST" [] )
                    ( "aST", Type.namedWith [] "AST" [] )
                    aSTTags.if_
                , Elm.Case.branch2
                    "Let"
                    ( "list.List"
                    , Type.list
                        (Type.tuple
                            (Type.namedWith [] "Token" [])
                            (Type.namedWith [] "AST" [])
                        )
                    )
                    ( "aST", Type.namedWith [] "AST" [] )
                    aSTTags.let_
                , Elm.Case.branch2
                    "Letrec"
                    ( "list.List"
                    , Type.list
                        (Type.tuple
                            (Type.namedWith [] "Token" [])
                            (Type.namedWith [] "AST" [])
                        )
                    )
                    ( "aST", Type.namedWith [] "AST" [] )
                    aSTTags.letrec
                , Elm.Case.branch1
                    "Quote"
                    ( "cons", Type.namedWith [] "Cons" [ Type.int ] )
                    aSTTags.quote
                , Elm.Case.branch1 "Val" ( "basics.Int", Type.int ) aSTTags.val
                , Elm.Case.branch1
                    "Var"
                    ( "token", Type.namedWith [] "Token" [] )
                    aSTTags.var
                , Elm.Case.branch0 "Truthy" aSTTags.truthy
                ]
    }


call_ :
    { tokensHelp : Elm.Expression -> Elm.Expression -> Elm.Expression
    , parseList : Elm.Expression -> Elm.Expression
    , quoteInParensHelper : Elm.Expression -> Elm.Expression
    , parseGeneralLet : Elm.Expression -> Elm.Expression -> Elm.Expression
    , parse : Elm.Expression -> Elm.Expression
    , token : Elm.Expression -> Elm.Expression
    , int : Elm.Expression -> Elm.Expression
    , var : Elm.Expression -> Elm.Expression
    , tokenToStr : Elm.Expression -> Elm.Expression
    }
call_ =
    { tokensHelp =
        \tokensHelpArg tokensHelpArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "tokensHelp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Parser" [ Type.var "a" ]
                                , Type.list (Type.var "a")
                                ]
                                (Type.namedWith
                                    []
                                    "Parser"
                                    [ Type.namedWith
                                        []
                                        "Step"
                                        [ Type.list (Type.var "a")
                                        , Type.list (Type.var "a")
                                        ]
                                    ]
                                )
                            )
                    }
                )
                [ tokensHelpArg, tokensHelpArg0 ]
    , parseList =
        \parseListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "parseList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Parser" [ Type.var "a" ] ]
                                (Type.namedWith
                                    []
                                    "Parser"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ parseListArg ]
    , quoteInParensHelper =
        \quoteInParensHelperArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "quoteInParensHelper"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [] "Cons" [ Type.int ])
                                ]
                                (Type.namedWith
                                    []
                                    "Parser"
                                    [ Type.namedWith
                                        []
                                        "Step"
                                        [ Type.list
                                            (Type.namedWith
                                                []
                                                "Cons"
                                                [ Type.int ]
                                            )
                                        , Type.list
                                            (Type.namedWith
                                                []
                                                "Cons"
                                                [ Type.int ]
                                            )
                                        ]
                                    ]
                                )
                            )
                    }
                )
                [ quoteInParensHelperArg ]
    , parseGeneralLet =
        \parseGeneralLetArg parseGeneralLetArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "parseGeneralLet"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function
                                    [ Type.list
                                        (Type.tuple
                                            (Type.namedWith [] "Token" [])
                                            (Type.namedWith [] "AST" [])
                                        )
                                    , Type.namedWith [] "AST" []
                                    ]
                                    (Type.namedWith [] "AST" [])
                                ]
                                (Type.namedWith
                                    []
                                    "Parser"
                                    [ Type.namedWith [] "AST" [] ]
                                )
                            )
                    }
                )
                [ parseGeneralLetArg, parseGeneralLetArg0 ]
    , parse =
        \parseArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "parse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.list (Type.namedWith [] "DeadEnd" [])
                                    , Type.namedWith [] "AST" []
                                    ]
                                )
                            )
                    }
                )
                [ parseArg ]
    , token =
        \tokenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "token"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [] "Token" [])
                            )
                    }
                )
                [ tokenArg ]
    , int =
        \intArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "int"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [] "AST" [])
                            )
                    }
                )
                [ intArg ]
    , var =
        \varArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "var"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [] "AST" [])
                            )
                    }
                )
                [ varArg ]
    , tokenToStr =
        \tokenToStrArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "LispAST" ]
                    , name = "tokenToStr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Token" [] ]
                                Type.string
                            )
                    }
                )
                [ tokenToStrArg ]
    }


values_ :
    { tokensHelp : Elm.Expression
    , parseList : Elm.Expression
    , spaces : Elm.Expression
    , parseComment : Elm.Expression
    , parseNil : Elm.Expression
    , parseNumeral : Elm.Expression
    , parseInt : Elm.Expression
    , parseValue : Elm.Expression
    , quoteInParensHelper : Elm.Expression
    , quoteParseInParens : Elm.Expression
    , quoteParseCons : Elm.Expression
    , parseQuote : Elm.Expression
    , parseGeneralLet : Elm.Expression
    , parseLetrec : Elm.Expression
    , parseLet : Elm.Expression
    , parseLambda : Elm.Expression
    , parseTruthy : Elm.Expression
    , parseToken : Elm.Expression
    , parseVariable : Elm.Expression
    , parseIf : Elm.Expression
    , parseFunctionApp : Elm.Expression
    , parser : Elm.Expression
    , parse : Elm.Expression
    , token : Elm.Expression
    , int : Elm.Expression
    , var : Elm.Expression
    , nil : Elm.Expression
    , tokenToStr : Elm.Expression
    }
values_ =
    { tokensHelp =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "tokensHelp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Parser" [ Type.var "a" ]
                        , Type.list (Type.var "a")
                        ]
                        (Type.namedWith
                            []
                            "Parser"
                            [ Type.namedWith
                                []
                                "Step"
                                [ Type.list (Type.var "a")
                                , Type.list (Type.var "a")
                                ]
                            ]
                        )
                    )
            }
    , parseList =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Parser" [ Type.var "a" ] ]
                        (Type.namedWith [] "Parser" [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , spaces =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "spaces"
            , annotation = Just (Type.namedWith [] "Parser" [ Type.unit ])
            }
    , parseComment =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseComment"
            , annotation = Just (Type.namedWith [] "Parser" [ Type.unit ])
            }
    , parseNil =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseNil"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseNumeral =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseNumeral"
            , annotation = Just (Type.namedWith [] "Parser" [ Type.int ])
            }
    , parseInt =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseInt"
            , annotation = Just (Type.namedWith [] "Parser" [ Type.int ])
            }
    , parseValue =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseValue"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , quoteInParensHelper =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "quoteInParensHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Cons" [ Type.int ]) ]
                        (Type.namedWith
                            []
                            "Parser"
                            [ Type.namedWith
                                []
                                "Step"
                                [ Type.list
                                    (Type.namedWith [] "Cons" [ Type.int ])
                                , Type.list
                                    (Type.namedWith [] "Cons" [ Type.int ])
                                ]
                            ]
                        )
                    )
            }
    , quoteParseInParens =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "quoteParseInParens"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Parser"
                        [ Type.namedWith [] "Cons" [ Type.int ] ]
                    )
            }
    , quoteParseCons =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "quoteParseCons"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Parser"
                        [ Type.namedWith [] "Cons" [ Type.int ] ]
                    )
            }
    , parseQuote =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseQuote"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseGeneralLet =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseGeneralLet"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.list
                                (Type.tuple
                                    (Type.namedWith [] "Token" [])
                                    (Type.namedWith [] "AST" [])
                                )
                            , Type.namedWith [] "AST" []
                            ]
                            (Type.namedWith [] "AST" [])
                        ]
                        (Type.namedWith
                            []
                            "Parser"
                            [ Type.namedWith [] "AST" [] ]
                        )
                    )
            }
    , parseLetrec =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseLetrec"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseLet =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseLet"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseLambda =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseLambda"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseTruthy =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseTruthy"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseToken =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseToken"
            , annotation =
                Just
                    (Type.namedWith [] "Parser" [ Type.namedWith [] "Token" [] ]
                    )
            }
    , parseVariable =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseVariable"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseIf =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseIf"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parseFunctionApp =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parseFunctionApp"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parser =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parser"
            , annotation =
                Just (Type.namedWith [] "Parser" [ Type.namedWith [] "AST" [] ])
            }
    , parse =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "parse"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.list (Type.namedWith [] "DeadEnd" [])
                            , Type.namedWith [] "AST" []
                            ]
                        )
                    )
            }
    , token =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "token"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Token" [])
                    )
            }
    , int =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "int"
            , annotation =
                Just (Type.function [ Type.int ] (Type.namedWith [] "AST" []))
            }
    , var =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "var"
            , annotation =
                Just
                    (Type.function [ Type.string ] (Type.namedWith [] "AST" []))
            }
    , nil =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "nil"
            , annotation = Just (Type.namedWith [] "AST" [])
            }
    , tokenToStr =
        Elm.value
            { importFrom = [ "Lib", "LispAST" ]
            , name = "tokenToStr"
            , annotation =
                Just
                    (Type.function [ Type.namedWith [] "Token" [] ] Type.string)
            }
    }


