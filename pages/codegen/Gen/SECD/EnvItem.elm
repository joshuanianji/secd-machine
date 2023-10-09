module Gen.SECD.EnvItem exposing (annotation_, call_, caseOf_, decoder, encode, make_, moduleName_, values_, view)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, view, encode, decoder, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "SECD", "EnvItem" ]


{-| decoder: Decoder a -> Decoder (EnvItem a) -}
decoder : Elm.Expression -> Elm.Expression
decoder decoderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "EnvItem" ]
            , name = "decoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Decoder" [ Type.var "a" ] ]
                        (Type.namedWith
                            []
                            "Decoder"
                            [ Type.namedWith [] "EnvItem" [ Type.var "a" ] ]
                        )
                    )
            }
        )
        [ decoderArg ]


{-| encode: (a -> Value) -> EnvItem a -> Value -}
encode : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
encode encodeArg encodeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "EnvItem" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Value" [])
                        , Type.namedWith [] "EnvItem" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Value" [])
                    )
            }
        )
        [ Elm.functionReduced "encodeUnpack" encodeArg, encodeArg0 ]


{-| view: (a -> Element msg) -> EnvItem a -> Element msg -}
view : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "EnvItem" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        , Type.namedWith [] "EnvItem" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "viewUnpack" viewArg, viewArg0 ]


annotation_ : { envItem : Type.Annotation -> Type.Annotation }
annotation_ =
    { envItem =
        \envItemArg0 ->
            Type.namedWith [ "SECD", "EnvItem" ] "EnvItem" [ envItemArg0 ]
    }


make_ : { listItem : Elm.Expression -> Elm.Expression, dummy : Elm.Expression }
make_ =
    { listItem =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "EnvItem" ]
                    , name = "ListItem"
                    , annotation =
                        Just (Type.namedWith [] "EnvItem" [ Type.var "a" ])
                    }
                )
                [ ar0 ]
    , dummy =
        Elm.value
            { importFrom = [ "SECD", "EnvItem" ]
            , name = "Dummy"
            , annotation = Just (Type.namedWith [] "EnvItem" [ Type.var "a" ])
            }
    }


caseOf_ :
    { envItem :
        Elm.Expression
        -> { envItemTags_0_0
            | listItem : Elm.Expression -> Elm.Expression
            , dummy : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { envItem =
        \envItemExpression envItemTags ->
            Elm.Case.custom
                envItemExpression
                (Type.namedWith [ "SECD", "EnvItem" ] "EnvItem" [ Type.var "a" ]
                )
                [ Elm.Case.branch1
                    "ListItem"
                    ( "list.List"
                    , Type.list (Type.namedWith [] "Cons" [ Type.var "a" ])
                    )
                    envItemTags.listItem
                , Elm.Case.branch0 "Dummy" envItemTags.dummy
                ]
    }


call_ :
    { decoder : Elm.Expression -> Elm.Expression
    , encode : Elm.Expression -> Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { decoder =
        \decoderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "EnvItem" ]
                    , name = "decoder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Decoder" [ Type.var "a" ] ]
                                (Type.namedWith
                                    []
                                    "Decoder"
                                    [ Type.namedWith
                                        []
                                        "EnvItem"
                                        [ Type.var "a" ]
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
                    { importFrom = [ "SECD", "EnvItem" ]
                    , name = "encode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith [] "Value" [])
                                , Type.namedWith [] "EnvItem" [ Type.var "a" ]
                                ]
                                (Type.namedWith [] "Value" [])
                            )
                    }
                )
                [ encodeArg, encodeArg0 ]
    , view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "EnvItem" ]
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
                                , Type.namedWith [] "EnvItem" [ Type.var "a" ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    }


values_ :
    { decoder : Elm.Expression, encode : Elm.Expression, view : Elm.Expression }
values_ =
    { decoder =
        Elm.value
            { importFrom = [ "SECD", "EnvItem" ]
            , name = "decoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Decoder" [ Type.var "a" ] ]
                        (Type.namedWith
                            []
                            "Decoder"
                            [ Type.namedWith [] "EnvItem" [ Type.var "a" ] ]
                        )
                    )
            }
    , encode =
        Elm.value
            { importFrom = [ "SECD", "EnvItem" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Value" [])
                        , Type.namedWith [] "EnvItem" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Value" [])
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "SECD", "EnvItem" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        , Type.namedWith [] "EnvItem" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    }


