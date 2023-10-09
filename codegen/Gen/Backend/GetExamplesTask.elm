module Gen.Backend.GetExamplesTask exposing (annotation_, call_, exampleFile, exampleFileDecoder, examples, getDefault, make_, moduleName_, transformExampleGroupRaw, values_)

{-| 
@docs values_, call_, make_, annotation_, getDefault, examples, transformExampleGroupRaw, exampleFile, exampleFileDecoder, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Backend", "GetExamplesTask" ]


{-| exampleFileDecoder: String -> String -> Json.Decode.Decoder Example -}
exampleFileDecoder : String -> String -> Elm.Expression
exampleFileDecoder exampleFileDecoderArg exampleFileDecoderArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "exampleFileDecoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.namedWith [] "Example" [] ]
                        )
                    )
            }
        )
        [ Elm.string exampleFileDecoderArg, Elm.string exampleFileDecoderArg0 ]


{-| exampleFile: String -> String -> BackendTask FatalError Example -}
exampleFile : String -> String -> Elm.Expression
exampleFile exampleFileArg exampleFileArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "exampleFile"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            []
                            "BackendTask"
                            [ Type.namedWith [] "FatalError" []
                            , Type.namedWith [] "Example" []
                            ]
                        )
                    )
            }
        )
        [ Elm.string exampleFileArg, Elm.string exampleFileArg0 ]


{-| transformExampleGroupRaw: List ExampleGroupRaw -> BackendTask FatalError (Nonempty ExampleGroup) -}
transformExampleGroupRaw : List Elm.Expression -> Elm.Expression
transformExampleGroupRaw transformExampleGroupRawArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "transformExampleGroupRaw"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "ExampleGroupRaw" []) ]
                        (Type.namedWith
                            []
                            "BackendTask"
                            [ Type.namedWith [] "FatalError" []
                            , Type.namedWith
                                []
                                "Nonempty"
                                [ Type.namedWith [] "ExampleGroup" [] ]
                            ]
                        )
                    )
            }
        )
        [ Elm.list transformExampleGroupRawArg ]


{-| examples: BackendTask FatalError (Nonempty ExampleGroup) -}
examples : Elm.Expression
examples =
    Elm.value
        { importFrom = [ "Backend", "GetExamplesTask" ]
        , name = "examples"
        , annotation =
            Just
                (Type.namedWith
                    []
                    "BackendTask"
                    [ Type.namedWith [] "FatalError" []
                    , Type.namedWith
                        []
                        "Nonempty"
                        [ Type.namedWith [] "ExampleGroup" [] ]
                    ]
                )
        }


{-| getDefault: Nonempty ExampleGroup -> ( String, Example ) -}
getDefault : Elm.Expression -> Elm.Expression
getDefault getDefaultArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "getDefault"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            []
                            "Nonempty"
                            [ Type.namedWith [] "ExampleGroup" [] ]
                        ]
                        (Type.tuple Type.string (Type.namedWith [] "Example" [])
                        )
                    )
            }
        )
        [ getDefaultArg ]


annotation_ :
    { exampleGroupRaw : Type.Annotation
    , example : Type.Annotation
    , exampleGroup : Type.Annotation
    }
annotation_ =
    { exampleGroupRaw =
        Type.alias
            moduleName_
            "ExampleGroupRaw"
            []
            (Type.record
                [ ( "path", Type.string )
                , ( "groupOrder", Type.int )
                , ( "groupName", Type.string )
                , ( "fileName", Type.string )
                ]
            )
    , example =
        Type.alias
            moduleName_
            "Example"
            []
            (Type.record
                [ ( "fileName", Type.string )
                , ( "code", Type.string )
                , ( "name", Type.string )
                ]
            )
    , exampleGroup =
        Type.alias
            moduleName_
            "ExampleGroup"
            []
            (Type.record
                [ ( "groupName", Type.string )
                , ( "examples"
                  , Type.namedWith
                        []
                        "Nonempty"
                        [ Type.namedWith [] "Example" [] ]
                  )
                ]
            )
    }


make_ :
    { exampleGroupRaw :
        { path : Elm.Expression
        , groupOrder : Elm.Expression
        , groupName : Elm.Expression
        , fileName : Elm.Expression
        }
        -> Elm.Expression
    , example :
        { fileName : Elm.Expression
        , code : Elm.Expression
        , name : Elm.Expression
        }
        -> Elm.Expression
    , exampleGroup :
        { groupName : Elm.Expression, examples : Elm.Expression }
        -> Elm.Expression
    }
make_ =
    { exampleGroupRaw =
        \exampleGroupRaw_args ->
            Elm.withType
                (Type.alias
                    [ "Backend", "GetExamplesTask" ]
                    "ExampleGroupRaw"
                    []
                    (Type.record
                        [ ( "path", Type.string )
                        , ( "groupOrder", Type.int )
                        , ( "groupName", Type.string )
                        , ( "fileName", Type.string )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "path" exampleGroupRaw_args.path
                    , Tuple.pair "groupOrder" exampleGroupRaw_args.groupOrder
                    , Tuple.pair "groupName" exampleGroupRaw_args.groupName
                    , Tuple.pair "fileName" exampleGroupRaw_args.fileName
                    ]
                )
    , example =
        \example_args ->
            Elm.withType
                (Type.alias
                    [ "Backend", "GetExamplesTask" ]
                    "Example"
                    []
                    (Type.record
                        [ ( "fileName", Type.string )
                        , ( "code", Type.string )
                        , ( "name", Type.string )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "fileName" example_args.fileName
                    , Tuple.pair "code" example_args.code
                    , Tuple.pair "name" example_args.name
                    ]
                )
    , exampleGroup =
        \exampleGroup_args ->
            Elm.withType
                (Type.alias
                    [ "Backend", "GetExamplesTask" ]
                    "ExampleGroup"
                    []
                    (Type.record
                        [ ( "groupName", Type.string )
                        , ( "examples"
                          , Type.namedWith
                                []
                                "Nonempty"
                                [ Type.namedWith [] "Example" [] ]
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "groupName" exampleGroup_args.groupName
                    , Tuple.pair "examples" exampleGroup_args.examples
                    ]
                )
    }


call_ :
    { exampleFileDecoder : Elm.Expression -> Elm.Expression -> Elm.Expression
    , exampleFile : Elm.Expression -> Elm.Expression -> Elm.Expression
    , transformExampleGroupRaw : Elm.Expression -> Elm.Expression
    , getDefault : Elm.Expression -> Elm.Expression
    }
call_ =
    { exampleFileDecoder =
        \exampleFileDecoderArg exampleFileDecoderArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Backend", "GetExamplesTask" ]
                    , name = "exampleFileDecoder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.namedWith [] "Example" [] ]
                                )
                            )
                    }
                )
                [ exampleFileDecoderArg, exampleFileDecoderArg0 ]
    , exampleFile =
        \exampleFileArg exampleFileArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Backend", "GetExamplesTask" ]
                    , name = "exampleFile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    []
                                    "BackendTask"
                                    [ Type.namedWith [] "FatalError" []
                                    , Type.namedWith [] "Example" []
                                    ]
                                )
                            )
                    }
                )
                [ exampleFileArg, exampleFileArg0 ]
    , transformExampleGroupRaw =
        \transformExampleGroupRawArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Backend", "GetExamplesTask" ]
                    , name = "transformExampleGroupRaw"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [] "ExampleGroupRaw" [])
                                ]
                                (Type.namedWith
                                    []
                                    "BackendTask"
                                    [ Type.namedWith [] "FatalError" []
                                    , Type.namedWith
                                        []
                                        "Nonempty"
                                        [ Type.namedWith [] "ExampleGroup" [] ]
                                    ]
                                )
                            )
                    }
                )
                [ transformExampleGroupRawArg ]
    , getDefault =
        \getDefaultArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Backend", "GetExamplesTask" ]
                    , name = "getDefault"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    []
                                    "Nonempty"
                                    [ Type.namedWith [] "ExampleGroup" [] ]
                                ]
                                (Type.tuple
                                    Type.string
                                    (Type.namedWith [] "Example" [])
                                )
                            )
                    }
                )
                [ getDefaultArg ]
    }


values_ :
    { exampleFileDecoder : Elm.Expression
    , exampleFile : Elm.Expression
    , transformExampleGroupRaw : Elm.Expression
    , examples : Elm.Expression
    , getDefault : Elm.Expression
    }
values_ =
    { exampleFileDecoder =
        Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "exampleFileDecoder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.namedWith [] "Example" [] ]
                        )
                    )
            }
    , exampleFile =
        Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "exampleFile"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            []
                            "BackendTask"
                            [ Type.namedWith [] "FatalError" []
                            , Type.namedWith [] "Example" []
                            ]
                        )
                    )
            }
    , transformExampleGroupRaw =
        Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "transformExampleGroupRaw"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "ExampleGroupRaw" []) ]
                        (Type.namedWith
                            []
                            "BackendTask"
                            [ Type.namedWith [] "FatalError" []
                            , Type.namedWith
                                []
                                "Nonempty"
                                [ Type.namedWith [] "ExampleGroup" [] ]
                            ]
                        )
                    )
            }
    , examples =
        Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "examples"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "BackendTask"
                        [ Type.namedWith [] "FatalError" []
                        , Type.namedWith
                            []
                            "Nonempty"
                            [ Type.namedWith [] "ExampleGroup" [] ]
                        ]
                    )
            }
    , getDefault =
        Elm.value
            { importFrom = [ "Backend", "GetExamplesTask" ]
            , name = "getDefault"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            []
                            "Nonempty"
                            [ Type.namedWith [] "ExampleGroup" [] ]
                        ]
                        (Type.tuple Type.string (Type.namedWith [] "Example" [])
                        )
                    )
            }
    }


