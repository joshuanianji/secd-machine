module Gen.Flags exposing (annotation_, call_, decoder, findCodeExample, make_, moduleName_, values_)

{-| 
@docs values_, call_, make_, annotation_, decoder, findCodeExample, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Flags" ]


{-| findCodeExample: String -> CodeExamples -> Maybe ( String, String ) -}
findCodeExample : String -> Elm.Expression -> Elm.Expression
findCodeExample findCodeExampleArg findCodeExampleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Flags" ]
            , name = "findCodeExample"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.namedWith [] "CodeExamples" [] ]
                        (Type.namedWith
                            []
                            "Maybe"
                            [ Type.tuple Type.string Type.string ]
                        )
                    )
            }
        )
        [ Elm.string findCodeExampleArg, findCodeExampleArg0 ]


{-| decoder: Decoder Flags -}
decoder : Elm.Expression
decoder =
    Elm.value
        { importFrom = [ "Flags" ]
        , name = "decoder"
        , annotation =
            Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "Flags" [] ])
        }


annotation_ :
    { defaultExample : Type.Annotation
    , screen : Type.Annotation
    , codeExamples : Type.Annotation
    , flags : Type.Annotation
    }
annotation_ =
    { defaultExample =
        Type.alias
            moduleName_
            "DefaultExample"
            []
            (Type.record
                [ ( "name", Type.string )
                , ( "code", Type.string )
                , ( "tab", Type.string )
                ]
            )
    , screen =
        Type.alias
            moduleName_
            "Screen"
            []
            (Type.record [ ( "width", Type.int ), ( "height", Type.int ) ])
    , codeExamples =
        Type.alias
            moduleName_
            "CodeExamples"
            []
            (Type.list
                (Type.tuple
                    Type.string
                    (Type.list (Type.tuple Type.string Type.string))
                )
            )
    , flags =
        Type.alias
            moduleName_
            "Flags"
            []
            (Type.record
                [ ( "codeExamples", Type.namedWith [] "CodeExamples" [] )
                , ( "defaultExample", Type.namedWith [] "DefaultExample" [] )
                ]
            )
    }


make_ :
    { defaultExample :
        { name : Elm.Expression, code : Elm.Expression, tab : Elm.Expression }
        -> Elm.Expression
    , screen :
        { width : Elm.Expression, height : Elm.Expression } -> Elm.Expression
    , flags :
        { codeExamples : Elm.Expression, defaultExample : Elm.Expression }
        -> Elm.Expression
    }
make_ =
    { defaultExample =
        \defaultExample_args ->
            Elm.withType
                (Type.alias
                    [ "Flags" ]
                    "DefaultExample"
                    []
                    (Type.record
                        [ ( "name", Type.string )
                        , ( "code", Type.string )
                        , ( "tab", Type.string )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "name" defaultExample_args.name
                    , Tuple.pair "code" defaultExample_args.code
                    , Tuple.pair "tab" defaultExample_args.tab
                    ]
                )
    , screen =
        \screen_args ->
            Elm.withType
                (Type.alias
                    [ "Flags" ]
                    "Screen"
                    []
                    (Type.record
                        [ ( "width", Type.int ), ( "height", Type.int ) ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "width" screen_args.width
                    , Tuple.pair "height" screen_args.height
                    ]
                )
    , flags =
        \flags_args ->
            Elm.withType
                (Type.alias
                    [ "Flags" ]
                    "Flags"
                    []
                    (Type.record
                        [ ( "codeExamples"
                          , Type.namedWith [] "CodeExamples" []
                          )
                        , ( "defaultExample"
                          , Type.namedWith [] "DefaultExample" []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "codeExamples" flags_args.codeExamples
                    , Tuple.pair "defaultExample" flags_args.defaultExample
                    ]
                )
    }


call_ : { findCodeExample : Elm.Expression -> Elm.Expression -> Elm.Expression }
call_ =
    { findCodeExample =
        \findCodeExampleArg findCodeExampleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Flags" ]
                    , name = "findCodeExample"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith [] "CodeExamples" []
                                ]
                                (Type.namedWith
                                    []
                                    "Maybe"
                                    [ Type.tuple Type.string Type.string ]
                                )
                            )
                    }
                )
                [ findCodeExampleArg, findCodeExampleArg0 ]
    }


values_ : { findCodeExample : Elm.Expression, decoder : Elm.Expression }
values_ =
    { findCodeExample =
        Elm.value
            { importFrom = [ "Flags" ]
            , name = "findCodeExample"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.namedWith [] "CodeExamples" [] ]
                        (Type.namedWith
                            []
                            "Maybe"
                            [ Type.tuple Type.string Type.string ]
                        )
                    )
            }
    , decoder =
        Elm.value
            { importFrom = [ "Flags" ]
            , name = "decoder"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Decoder"
                        [ Type.namedWith [] "Flags" [] ]
                    )
            }
    }


