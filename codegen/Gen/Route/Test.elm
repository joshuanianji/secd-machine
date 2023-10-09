module Gen.Route.Test exposing (annotation_, make_, moduleName_, route, values_)

{-| 
@docs values_, make_, annotation_, route, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Route", "Test" ]


{-| route: RouteBuilder.StatefulRoute RouteParams Data ActionData Model Msg -}
route : Elm.Expression
route =
    Elm.value
        { importFrom = [ "Route", "Test" ]
        , name = "route"
        , annotation =
            Just
                (Type.namedWith
                    [ "RouteBuilder" ]
                    "StatefulRoute"
                    [ Type.namedWith [] "RouteParams" []
                    , Type.namedWith [] "Data" []
                    , Type.namedWith [] "ActionData" []
                    , Type.namedWith [] "Model" []
                    , Type.namedWith [] "Msg" []
                    ]
                )
        }


annotation_ :
    { actionData : Type.Annotation
    , data : Type.Annotation
    , routeParams : Type.Annotation
    , model : Type.Annotation
    , msg : Type.Annotation
    }
annotation_ =
    { actionData =
        Type.alias
            moduleName_
            "ActionData"
            []
            (Type.record
                [ ( "errors"
                  , Type.namedWith [ "Form" ] "ServerResponse" [ Type.string ]
                  )
                ]
            )
    , data = Type.alias moduleName_ "Data" [] (Type.record [])
    , routeParams = Type.alias moduleName_ "RouteParams" [] (Type.record [])
    , model = Type.alias moduleName_ "Model" [] (Type.record [])
    , msg = Type.namedWith [ "Route", "Test" ] "Msg" []
    }


make_ :
    { actionData : { errors : Elm.Expression } -> Elm.Expression
    , data : data -> Elm.Expression
    , routeParams : routeParams -> Elm.Expression
    , model : model -> Elm.Expression
    }
make_ =
    { actionData =
        \actionData_args ->
            Elm.withType
                (Type.alias
                    [ "Route", "Test" ]
                    "ActionData"
                    []
                    (Type.record
                        [ ( "errors"
                          , Type.namedWith
                                [ "Form" ]
                                "ServerResponse"
                                [ Type.string ]
                          )
                        ]
                    )
                )
                (Elm.record [ Tuple.pair "errors" actionData_args.errors ])
    , data =
        \data_args ->
            Elm.withType
                (Type.alias [ "Route", "Test" ] "Data" [] (Type.record []))
                (Elm.record [])
    , routeParams =
        \routeParams_args ->
            Elm.withType
                (Type.alias
                    [ "Route", "Test" ]
                    "RouteParams"
                    []
                    (Type.record [])
                )
                (Elm.record [])
    , model =
        \model_args ->
            Elm.withType
                (Type.alias [ "Route", "Test" ] "Model" [] (Type.record []))
                (Elm.record [])
    }


values_ : { route : Elm.Expression }
values_ =
    { route =
        Elm.value
            { importFrom = [ "Route", "Test" ]
            , name = "route"
            , annotation =
                Just
                    (Type.namedWith
                        [ "RouteBuilder" ]
                        "StatefulRoute"
                        [ Type.namedWith [] "RouteParams" []
                        , Type.namedWith [] "Data" []
                        , Type.namedWith [] "ActionData" []
                        , Type.namedWith [] "Model" []
                        , Type.namedWith [] "Msg" []
                        ]
                    )
            }
    }


