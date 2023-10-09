module Gen.Views.VM exposing (annotation_, call_, init, make_, moduleName_, subscriptions, update, values_, view)

{-| 
@docs values_, call_, make_, annotation_, init, update, view, subscriptions, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Views", "VM" ]


{-| subscriptions: Model -> Sub Msg -}
subscriptions : Elm.Expression -> Elm.Expression
subscriptions subscriptionsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "subscriptions"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Model" [] ]
                        (Type.namedWith [] "Sub" [ Type.namedWith [] "Msg" [] ])
                    )
            }
        )
        [ subscriptionsArg ]


{-| view: Model -> Element Msg -}
view : Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Model" [] ]
                        (Type.namedWith
                            []
                            "Element"
                            [ Type.namedWith [] "Msg" [] ]
                        )
                    )
            }
        )
        [ viewArg ]


{-| update: Msg -> Model -> ( Model, Cmd Msg ) -}
update : Elm.Expression -> Elm.Expression -> Elm.Expression
update updateArg updateArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Msg" []
                        , Type.namedWith [] "Model" []
                        ]
                        (Type.tuple
                            (Type.namedWith [] "Model" [])
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith [] "Msg" [] ]
                            )
                        )
                    )
            }
        )
        [ updateArg, updateArg0 ]


{-| init: PagesInfo -> Program -> ( Model, Cmd Msg ) -}
init : Elm.Expression -> Elm.Expression -> Elm.Expression
init initArg initArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "PagesInfo" []
                        , Type.namedWith [] "Program" []
                        ]
                        (Type.tuple
                            (Type.namedWith [] "Model" [])
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith [] "Msg" [] ]
                            )
                        )
                    )
            }
        )
        [ initArg, initArg0 ]


annotation_ : { model : Type.Annotation, msg : Type.Annotation }
annotation_ =
    { model =
        Type.alias
            moduleName_
            "Model"
            []
            (Type.record
                [ ( "index", Type.int )
                , ( "stateSliderIdx", Type.int )
                , ( "chunk"
                  , Type.namedWith [] "Zipper" [ Type.namedWith [] "VM" [] ]
                  )
                , ( "page"
                  , Type.namedWith [] "Zipper" [ Type.namedWith [] "VM" [] ]
                  )
                , ( "pages", Type.namedWith [] "Zipper" [ Type.int ] )
                , ( "latestVM"
                  , Type.namedWith
                        []
                        "Result"
                        [ Type.namedWith [] "VM" []
                        , Type.namedWith [] "VMResult" []
                        ]
                  )
                , ( "totalStates", Type.int )
                , ( "pressedKeys", Type.list (Type.namedWith [] "Key" []) )
                , ( "pagesInfo", Type.namedWith [] "PagesInfo" [] )
                , ( "options", Type.namedWith [] "Options" [] )
                , ( "compiled", Type.namedWith [] "Program" [] )
                , ( "fetchStatus", Type.namedWith [] "FetchStatus" [] )
                ]
            )
    , msg = Type.namedWith [ "Views", "VM" ] "Msg" []
    }


make_ :
    { model :
        { index : Elm.Expression
        , stateSliderIdx : Elm.Expression
        , chunk : Elm.Expression
        , page : Elm.Expression
        , pages : Elm.Expression
        , latestVM : Elm.Expression
        , totalStates : Elm.Expression
        , pressedKeys : Elm.Expression
        , pagesInfo : Elm.Expression
        , options : Elm.Expression
        , compiled : Elm.Expression
        , fetchStatus : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { model =
        \model_args ->
            Elm.withType
                (Type.alias
                    [ "Views", "VM" ]
                    "Model"
                    []
                    (Type.record
                        [ ( "index", Type.int )
                        , ( "stateSliderIdx", Type.int )
                        , ( "chunk"
                          , Type.namedWith
                                []
                                "Zipper"
                                [ Type.namedWith [] "VM" [] ]
                          )
                        , ( "page"
                          , Type.namedWith
                                []
                                "Zipper"
                                [ Type.namedWith [] "VM" [] ]
                          )
                        , ( "pages", Type.namedWith [] "Zipper" [ Type.int ] )
                        , ( "latestVM"
                          , Type.namedWith
                                []
                                "Result"
                                [ Type.namedWith [] "VM" []
                                , Type.namedWith [] "VMResult" []
                                ]
                          )
                        , ( "totalStates", Type.int )
                        , ( "pressedKeys"
                          , Type.list (Type.namedWith [] "Key" [])
                          )
                        , ( "pagesInfo", Type.namedWith [] "PagesInfo" [] )
                        , ( "options", Type.namedWith [] "Options" [] )
                        , ( "compiled", Type.namedWith [] "Program" [] )
                        , ( "fetchStatus", Type.namedWith [] "FetchStatus" [] )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "index" model_args.index
                    , Tuple.pair "stateSliderIdx" model_args.stateSliderIdx
                    , Tuple.pair "chunk" model_args.chunk
                    , Tuple.pair "page" model_args.page
                    , Tuple.pair "pages" model_args.pages
                    , Tuple.pair "latestVM" model_args.latestVM
                    , Tuple.pair "totalStates" model_args.totalStates
                    , Tuple.pair "pressedKeys" model_args.pressedKeys
                    , Tuple.pair "pagesInfo" model_args.pagesInfo
                    , Tuple.pair "options" model_args.options
                    , Tuple.pair "compiled" model_args.compiled
                    , Tuple.pair "fetchStatus" model_args.fetchStatus
                    ]
                )
    }


call_ :
    { subscriptions : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression -> Elm.Expression
    , init : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { subscriptions =
        \subscriptionsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "VM" ]
                    , name = "subscriptions"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Model" [] ]
                                (Type.namedWith
                                    []
                                    "Sub"
                                    [ Type.namedWith [] "Msg" [] ]
                                )
                            )
                    }
                )
                [ subscriptionsArg ]
    , view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "VM" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Model" [] ]
                                (Type.namedWith
                                    []
                                    "Element"
                                    [ Type.namedWith [] "Msg" [] ]
                                )
                            )
                    }
                )
                [ viewArg ]
    , update =
        \updateArg updateArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "VM" ]
                    , name = "update"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Msg" []
                                , Type.namedWith [] "Model" []
                                ]
                                (Type.tuple
                                    (Type.namedWith [] "Model" [])
                                    (Type.namedWith
                                        []
                                        "Cmd"
                                        [ Type.namedWith [] "Msg" [] ]
                                    )
                                )
                            )
                    }
                )
                [ updateArg, updateArg0 ]
    , init =
        \initArg initArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "VM" ]
                    , name = "init"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "PagesInfo" []
                                , Type.namedWith [] "Program" []
                                ]
                                (Type.tuple
                                    (Type.namedWith [] "Model" [])
                                    (Type.namedWith
                                        []
                                        "Cmd"
                                        [ Type.namedWith [] "Msg" [] ]
                                    )
                                )
                            )
                    }
                )
                [ initArg, initArg0 ]
    }


values_ :
    { subscriptions : Elm.Expression
    , view : Elm.Expression
    , update : Elm.Expression
    , init : Elm.Expression
    }
values_ =
    { subscriptions =
        Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "subscriptions"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Model" [] ]
                        (Type.namedWith [] "Sub" [ Type.namedWith [] "Msg" [] ])
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Model" [] ]
                        (Type.namedWith
                            []
                            "Element"
                            [ Type.namedWith [] "Msg" [] ]
                        )
                    )
            }
    , update =
        Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Msg" []
                        , Type.namedWith [] "Model" []
                        ]
                        (Type.tuple
                            (Type.namedWith [] "Model" [])
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith [] "Msg" [] ]
                            )
                        )
                    )
            }
    , init =
        Elm.value
            { importFrom = [ "Views", "VM" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "PagesInfo" []
                        , Type.namedWith [] "Program" []
                        ]
                        (Type.tuple
                            (Type.namedWith [] "Model" [])
                            (Type.namedWith
                                []
                                "Cmd"
                                [ Type.namedWith [] "Msg" [] ]
                            )
                        )
                    )
            }
    }


