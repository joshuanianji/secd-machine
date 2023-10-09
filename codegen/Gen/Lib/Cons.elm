module Gen.Lib.Cons exposing (annotation_, call_, caseOf_, cons, decoder, encode, fromConsList, fromList, make_, moduleName_, nil, single, toList, toString, toStringHelper, values_, view, viewHelper)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, nil, single, cons, fromList, toList, fromConsList, toString, toStringHelper, view, viewHelper, encode, decoder, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Lib", "Cons" ]


{-| decoder: Decoder a -> Decoder (Cons a) -}
decoder : Elm.Expression -> Elm.Expression
decoder decoderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "decoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Decoder" [ Type.var "a" ] ]
                        (Type.namedWith
                            []
                            "Decoder"
                            [ Type.namedWith [] "Cons" [ Type.var "a" ] ]
                        )
                    )
            }
        )
        [ decoderArg ]


{-| encode: (a -> Value) -> Cons a -> Value -}
encode : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
encode encodeArg encodeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Value" [])
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Value" [])
                    )
            }
        )
        [ Elm.functionReduced "encodeUnpack" encodeArg, encodeArg0 ]


{-| viewHelper: (a -> Element msg) -> Cons a -> Element msg -}
viewHelper :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
viewHelper viewHelperArg viewHelperArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "viewHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "viewHelperUnpack" viewHelperArg, viewHelperArg0 ]


{-| view: (a -> Element msg) -> Cons a -> Element msg -}
view : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "viewUnpack" viewArg, viewArg0 ]


{-| toStringHelper: (a -> String) -> Cons a -> String -}
toStringHelper :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
toStringHelper toStringHelperArg toStringHelperArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "toStringHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.string
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
        )
        [ Elm.functionReduced "toStringHelperUnpack" toStringHelperArg
        , toStringHelperArg0
        ]


{-| toString: (a -> String) -> Cons a -> String -}
toString :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
toString toStringArg toStringArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "toString"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.string
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
        )
        [ Elm.functionReduced "toStringUnpack" toStringArg, toStringArg0 ]


{-| fromConsList: List (Cons a) -> Cons a -}
fromConsList : List Elm.Expression -> Elm.Expression
fromConsList fromConsListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "fromConsList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Cons" [ Type.var "a" ])
                        ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list fromConsListArg ]


{-| toList: Cons a -> Maybe (List (Cons a)) -}
toList : Elm.Expression -> Elm.Expression
toList toListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "toList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cons" [ Type.var "a" ] ]
                        (Type.namedWith
                            []
                            "Maybe"
                            [ Type.list
                                (Type.namedWith [] "Cons" [ Type.var "a" ])
                            ]
                        )
                    )
            }
        )
        [ toListArg ]


{-| fromList: List a -> Cons a -}
fromList : List Elm.Expression -> Elm.Expression
fromList fromListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "fromList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a") ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list fromListArg ]


{-| cons: Cons a -> Cons a -> Cons a -}
cons : Elm.Expression -> Elm.Expression -> Elm.Expression
cons consArg consArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "cons"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cons" [ Type.var "a" ]
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
        )
        [ consArg, consArg0 ]


{-| single: a -> Cons a -}
single : Elm.Expression -> Elm.Expression
single singleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "single"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
        )
        [ singleArg ]


{-| nil: Cons a -}
nil : Elm.Expression
nil =
    Elm.value
        { importFrom = [ "Lib", "Cons" ]
        , name = "nil"
        , annotation = Just (Type.namedWith [] "Cons" [ Type.var "a" ])
        }


annotation_ : { cons : Type.Annotation -> Type.Annotation }
annotation_ =
    { cons = \consArg0 -> Type.namedWith [ "Lib", "Cons" ] "Cons" [ consArg0 ] }


make_ :
    { nil : Elm.Expression
    , val : Elm.Expression -> Elm.Expression
    , cons : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
make_ =
    { nil =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "Nil"
            , annotation = Just (Type.namedWith [] "Cons" [ Type.var "a" ])
            }
    , val =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "Val"
                    , annotation =
                        Just (Type.namedWith [] "Cons" [ Type.var "a" ])
                    }
                )
                [ ar0 ]
    , cons =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "Cons"
                    , annotation =
                        Just (Type.namedWith [] "Cons" [ Type.var "a" ])
                    }
                )
                [ ar0, ar1 ]
    }


caseOf_ :
    { cons :
        Elm.Expression
        -> { consTags_0_0
            | nil : Elm.Expression
            , val : Elm.Expression -> Elm.Expression
            , cons : Elm.Expression -> Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { cons =
        \consExpression consTags ->
            Elm.Case.custom
                consExpression
                (Type.namedWith [ "Lib", "Cons" ] "Cons" [ Type.var "a" ])
                [ Elm.Case.branch0 "Nil" consTags.nil
                , Elm.Case.branch1 "Val" ( "a", Type.var "a" ) consTags.val
                , Elm.Case.branch2
                    "Cons"
                    ( "cons", Type.namedWith [] "Cons" [ Type.var "a" ] )
                    ( "cons", Type.namedWith [] "Cons" [ Type.var "a" ] )
                    consTags.cons
                ]
    }


call_ :
    { decoder : Elm.Expression -> Elm.Expression
    , encode : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewHelper : Elm.Expression -> Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toStringHelper : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toString : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fromConsList : Elm.Expression -> Elm.Expression
    , toList : Elm.Expression -> Elm.Expression
    , fromList : Elm.Expression -> Elm.Expression
    , cons : Elm.Expression -> Elm.Expression -> Elm.Expression
    , single : Elm.Expression -> Elm.Expression
    }
call_ =
    { decoder =
        \decoderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "decoder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Decoder" [ Type.var "a" ] ]
                                (Type.namedWith
                                    []
                                    "Decoder"
                                    [ Type.namedWith [] "Cons" [ Type.var "a" ]
                                    ]
                                )
                            )
                    }
                )
                [ decoderArg ]
    , encode =
        \encodeArg encodeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "encode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith [] "Value" [])
                                , Type.namedWith [] "Cons" [ Type.var "a" ]
                                ]
                                (Type.namedWith [] "Value" [])
                            )
                    }
                )
                [ encodeArg, encodeArg0 ]
    , viewHelper =
        \viewHelperArg viewHelperArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "viewHelper"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        []
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith [] "Cons" [ Type.var "a" ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewHelperArg, viewHelperArg0 ]
    , view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        []
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith [] "Cons" [ Type.var "a" ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    , toStringHelper =
        \toStringHelperArg toStringHelperArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "toStringHelper"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] Type.string
                                , Type.namedWith [] "Cons" [ Type.var "a" ]
                                ]
                                Type.string
                            )
                    }
                )
                [ toStringHelperArg, toStringHelperArg0 ]
    , toString =
        \toStringArg toStringArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "toString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] Type.string
                                , Type.namedWith [] "Cons" [ Type.var "a" ]
                                ]
                                Type.string
                            )
                    }
                )
                [ toStringArg, toStringArg0 ]
    , fromConsList =
        \fromConsListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "fromConsList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [] "Cons" [ Type.var "a" ])
                                ]
                                (Type.namedWith [] "Cons" [ Type.var "a" ])
                            )
                    }
                )
                [ fromConsListArg ]
    , toList =
        \toListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "toList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Cons" [ Type.var "a" ] ]
                                (Type.namedWith
                                    []
                                    "Maybe"
                                    [ Type.list
                                        (Type.namedWith
                                            []
                                            "Cons"
                                            [ Type.var "a" ]
                                        )
                                    ]
                                )
                            )
                    }
                )
                [ toListArg ]
    , fromList =
        \fromListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "fromList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.var "a") ]
                                (Type.namedWith [] "Cons" [ Type.var "a" ])
                            )
                    }
                )
                [ fromListArg ]
    , cons =
        \consArg consArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "cons"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Cons" [ Type.var "a" ]
                                , Type.namedWith [] "Cons" [ Type.var "a" ]
                                ]
                                (Type.namedWith [] "Cons" [ Type.var "a" ])
                            )
                    }
                )
                [ consArg, consArg0 ]
    , single =
        \singleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Cons" ]
                    , name = "single"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a" ]
                                (Type.namedWith [] "Cons" [ Type.var "a" ])
                            )
                    }
                )
                [ singleArg ]
    }


values_ :
    { decoder : Elm.Expression
    , encode : Elm.Expression
    , viewHelper : Elm.Expression
    , view : Elm.Expression
    , toStringHelper : Elm.Expression
    , toString : Elm.Expression
    , fromConsList : Elm.Expression
    , toList : Elm.Expression
    , fromList : Elm.Expression
    , cons : Elm.Expression
    , single : Elm.Expression
    , nil : Elm.Expression
    }
values_ =
    { decoder =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "decoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Decoder" [ Type.var "a" ] ]
                        (Type.namedWith
                            []
                            "Decoder"
                            [ Type.namedWith [] "Cons" [ Type.var "a" ] ]
                        )
                    )
            }
    , encode =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Value" [])
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Value" [])
                    )
            }
    , viewHelper =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "viewHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , toStringHelper =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "toStringHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.string
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
    , toString =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "toString"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.string
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        Type.string
                    )
            }
    , fromConsList =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "fromConsList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Cons" [ Type.var "a" ])
                        ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
    , toList =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "toList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cons" [ Type.var "a" ] ]
                        (Type.namedWith
                            []
                            "Maybe"
                            [ Type.list
                                (Type.namedWith [] "Cons" [ Type.var "a" ])
                            ]
                        )
                    )
            }
    , fromList =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "fromList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a") ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
    , cons =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "cons"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cons" [ Type.var "a" ]
                        , Type.namedWith [] "Cons" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
    , single =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "single"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
            }
    , nil =
        Elm.value
            { importFrom = [ "Lib", "Cons" ]
            , name = "nil"
            , annotation = Just (Type.namedWith [] "Cons" [ Type.var "a" ])
            }
    }


