module Gen.UrlState exposing (annotation_, call_, default, fromUrl, make_, merge, moduleName_, navigateTo, updateTab, values_)

{-| 
@docs values_, call_, make_, annotation_, default, updateTab, fromUrl, merge, navigateTo, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "UrlState" ]


{-| navigateTo: Nav.Key -> UrlState -> Cmd msg -}
navigateTo : Elm.Expression -> Elm.Expression -> Elm.Expression
navigateTo navigateToArg navigateToArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "UrlState" ]
            , name = "navigateTo"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nav" ] "Key" []
                        , Type.namedWith [] "UrlState" []
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ navigateToArg, navigateToArg0 ]


{-| merge: UrlState -> UrlState -> UrlState -}
merge : Elm.Expression -> Elm.Expression -> Elm.Expression
merge mergeArg mergeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "UrlState" ]
            , name = "merge"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "UrlState" []
                        , Type.namedWith [] "UrlState" []
                        ]
                        (Type.namedWith [] "UrlState" [])
                    )
            }
        )
        [ mergeArg, mergeArg0 ]


{-| fromUrl: Url -> UrlState -}
fromUrl : Elm.Expression -> Elm.Expression
fromUrl fromUrlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "UrlState" ]
            , name = "fromUrl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Url" [] ]
                        (Type.namedWith [] "UrlState" [])
                    )
            }
        )
        [ fromUrlArg ]


{-| updateTab: String -> Nav.Key -> UrlState -> ( UrlState, Cmd msg ) -}
updateTab : String -> Elm.Expression -> Elm.Expression -> Elm.Expression
updateTab updateTabArg updateTabArg0 updateTabArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "UrlState" ]
            , name = "updateTab"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nav" ] "Key" []
                        , Type.namedWith [] "UrlState" []
                        ]
                        (Type.tuple
                            (Type.namedWith [] "UrlState" [])
                            (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                        )
                    )
            }
        )
        [ Elm.string updateTabArg, updateTabArg0, updateTabArg1 ]


{-| default: UrlState -}
default : Elm.Expression
default =
    Elm.value
        { importFrom = [ "UrlState" ]
        , name = "default"
        , annotation = Just (Type.namedWith [] "UrlState" [])
        }


annotation_ : { urlState : Type.Annotation }
annotation_ =
    { urlState =
        Type.alias
            moduleName_
            "UrlState"
            []
            (Type.record [ ( "exampleName", Type.string ) ])
    }


make_ : { urlState : { exampleName : Elm.Expression } -> Elm.Expression }
make_ =
    { urlState =
        \urlState_args ->
            Elm.withType
                (Type.alias
                    [ "UrlState" ]
                    "UrlState"
                    []
                    (Type.record [ ( "exampleName", Type.string ) ])
                )
                (Elm.record
                    [ Tuple.pair "exampleName" urlState_args.exampleName ]
                )
    }


call_ :
    { navigateTo : Elm.Expression -> Elm.Expression -> Elm.Expression
    , merge : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fromUrl : Elm.Expression -> Elm.Expression
    , updateTab :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { navigateTo =
        \navigateToArg navigateToArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "UrlState" ]
                    , name = "navigateTo"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Nav" ] "Key" []
                                , Type.namedWith [] "UrlState" []
                                ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ navigateToArg, navigateToArg0 ]
    , merge =
        \mergeArg mergeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "UrlState" ]
                    , name = "merge"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "UrlState" []
                                , Type.namedWith [] "UrlState" []
                                ]
                                (Type.namedWith [] "UrlState" [])
                            )
                    }
                )
                [ mergeArg, mergeArg0 ]
    , fromUrl =
        \fromUrlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "UrlState" ]
                    , name = "fromUrl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Url" [] ]
                                (Type.namedWith [] "UrlState" [])
                            )
                    }
                )
                [ fromUrlArg ]
    , updateTab =
        \updateTabArg updateTabArg0 updateTabArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "UrlState" ]
                    , name = "updateTab"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith [ "Nav" ] "Key" []
                                , Type.namedWith [] "UrlState" []
                                ]
                                (Type.tuple
                                    (Type.namedWith [] "UrlState" [])
                                    (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                                )
                            )
                    }
                )
                [ updateTabArg, updateTabArg0, updateTabArg1 ]
    }


values_ :
    { navigateTo : Elm.Expression
    , merge : Elm.Expression
    , fromUrl : Elm.Expression
    , updateTab : Elm.Expression
    , default : Elm.Expression
    }
values_ =
    { navigateTo =
        Elm.value
            { importFrom = [ "UrlState" ]
            , name = "navigateTo"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nav" ] "Key" []
                        , Type.namedWith [] "UrlState" []
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , merge =
        Elm.value
            { importFrom = [ "UrlState" ]
            , name = "merge"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "UrlState" []
                        , Type.namedWith [] "UrlState" []
                        ]
                        (Type.namedWith [] "UrlState" [])
                    )
            }
    , fromUrl =
        Elm.value
            { importFrom = [ "UrlState" ]
            , name = "fromUrl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Url" [] ]
                        (Type.namedWith [] "UrlState" [])
                    )
            }
    , updateTab =
        Elm.value
            { importFrom = [ "UrlState" ]
            , name = "updateTab"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Nav" ] "Key" []
                        , Type.namedWith [] "UrlState" []
                        ]
                        (Type.tuple
                            (Type.namedWith [] "UrlState" [])
                            (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                        )
                    )
            }
    , default =
        Elm.value
            { importFrom = [ "UrlState" ]
            , name = "default"
            , annotation = Just (Type.namedWith [] "UrlState" [])
            }
    }


