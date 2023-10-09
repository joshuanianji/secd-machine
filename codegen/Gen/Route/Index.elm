module Gen.Route.Index exposing (annotation_, make_, moduleName_, route, values_)

{-| 
@docs values_, make_, annotation_, route, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Route", "Index" ]


{-| route: StatelessRoute RouteParams Data ActionData -}
route : Elm.Expression
route =
    Elm.value
        { importFrom = [ "Route", "Index" ]
        , name = "route"
        , annotation =
            Just
                (Type.namedWith
                    []
                    "StatelessRoute"
                    [ Type.namedWith [] "RouteParams" []
                    , Type.namedWith [] "Data" []
                    , Type.namedWith [] "ActionData" []
                    ]
                )
        }


annotation_ :
    { actionData : Type.Annotation
    , data : Type.Annotation
    , msg : Type.Annotation
    , model : Type.Annotation
    }
annotation_ =
    { actionData = Type.alias moduleName_ "ActionData" [] (Type.record [])
    , data =
        Type.alias
            moduleName_
            "Data"
            []
            (Type.record
                [ ( "exampleGroups"
                  , Type.namedWith
                        []
                        "Nonempty"
                        [ Type.namedWith
                            [ "Backend", "GetExamplesTask" ]
                            "ExampleGroup"
                            []
                        ]
                  )
                ]
            )
    , msg = Type.alias moduleName_ "Msg" [] Type.unit
    , model =
        Type.alias
            moduleName_
            "Model"
            []
            (Type.record
                [ ( "code", Type.string )
                , ( "openExampleTabs", Type.namedWith [] "Set" [ Type.string ] )
                , ( "openTabs", Type.namedWith [] "Set" [ Type.string ] )
                , ( "currCodeExample"
                  , Type.namedWith [] "Result" [ Type.string, Type.string ]
                  )
                , ( "compiled", Type.namedWith [] "CompiledState" [] )
                ]
            )
    }


make_ :
    { actionData : actionData -> Elm.Expression
    , data : { exampleGroups : Elm.Expression } -> Elm.Expression
    , model :
        { code : Elm.Expression
        , openExampleTabs : Elm.Expression
        , openTabs : Elm.Expression
        , currCodeExample : Elm.Expression
        , compiled : Elm.Expression
        }
        -> Elm.Expression
    }
make_ =
    { actionData =
        \actionData_args ->
            Elm.withType
                (Type.alias
                    [ "Route", "Index" ]
                    "ActionData"
                    []
                    (Type.record [])
                )
                (Elm.record [])
    , data =
        \data_args ->
            Elm.withType
                (Type.alias
                    [ "Route", "Index" ]
                    "Data"
                    []
                    (Type.record
                        [ ( "exampleGroups"
                          , Type.namedWith
                                []
                                "Nonempty"
                                [ Type.namedWith
                                    [ "Backend", "GetExamplesTask" ]
                                    "ExampleGroup"
                                    []
                                ]
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "exampleGroups" data_args.exampleGroups ]
                )
    , model =
        \model_args ->
            Elm.withType
                (Type.alias
                    [ "Route", "Index" ]
                    "Model"
                    []
                    (Type.record
                        [ ( "code", Type.string )
                        , ( "openExampleTabs"
                          , Type.namedWith [] "Set" [ Type.string ]
                          )
                        , ( "openTabs"
                          , Type.namedWith [] "Set" [ Type.string ]
                          )
                        , ( "currCodeExample"
                          , Type.namedWith
                                []
                                "Result"
                                [ Type.string, Type.string ]
                          )
                        , ( "compiled", Type.namedWith [] "CompiledState" [] )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "code" model_args.code
                    , Tuple.pair "openExampleTabs" model_args.openExampleTabs
                    , Tuple.pair "openTabs" model_args.openTabs
                    , Tuple.pair "currCodeExample" model_args.currCodeExample
                    , Tuple.pair "compiled" model_args.compiled
                    ]
                )
    }


values_ : { route : Elm.Expression }
values_ =
    { route =
        Elm.value
            { importFrom = [ "Route", "Index" ]
            , name = "route"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "StatelessRoute"
                        [ Type.namedWith [] "RouteParams" []
                        , Type.namedWith [] "Data" []
                        , Type.namedWith [] "ActionData" []
                        ]
                    )
            }
    }


