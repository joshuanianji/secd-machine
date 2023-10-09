module Gen.Ports exposing (blurs, call_, fetchPage, fetchPageResponse, initialize, log, moduleName_, sendPages, updateCode, updatedEditor, values_)

{-| 
@docs values_, call_, updatedEditor, updateCode, initialize, sendPages, fetchPage, fetchPageResponse, log, blurs, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Ports" ]


{-| blurs: (() -> msg) -> Sub msg -}
blurs : (Elm.Expression -> Elm.Expression) -> Elm.Expression
blurs blursArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "blurs"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.unit ] (Type.var "msg") ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "blursUnpack" blursArg ]


{-| log: String -> Cmd msg -}
log : String -> Elm.Expression
log logArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "log"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.string logArg ]


{-| fetchPageResponse: (( Int, Value ) -> msg) -> Sub msg -}
fetchPageResponse : (Elm.Expression -> Elm.Expression) -> Elm.Expression
fetchPageResponse fetchPageResponseArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "fetchPageResponse"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.tuple Type.int (Type.namedWith [] "Value" [])
                            ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "fetchPageResponseUnpack" fetchPageResponseArg ]


{-| fetchPage: Int -> Cmd msg -}
fetchPage : Int -> Elm.Expression
fetchPage fetchPageArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "fetchPage"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.int fetchPageArg ]


{-| sendPages: List ( Int, Value ) -> Cmd msg -}
sendPages : List Elm.Expression -> Elm.Expression
sendPages sendPagesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "sendPages"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple Type.int (Type.namedWith [] "Value" []))
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list sendPagesArg ]


{-| initialize: String -> Cmd msg -}
initialize : String -> Elm.Expression
initialize initializeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "initialize"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.string initializeArg ]


{-| updateCode: String -> Cmd msg -}
updateCode : String -> Elm.Expression
updateCode updateCodeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "updateCode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.string updateCodeArg ]


{-| updatedEditor: (String -> msg) -> Sub msg -}
updatedEditor : (Elm.Expression -> Elm.Expression) -> Elm.Expression
updatedEditor updatedEditorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "updatedEditor"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "updatedEditorUnpack" updatedEditorArg ]


call_ :
    { blurs : Elm.Expression -> Elm.Expression
    , log : Elm.Expression -> Elm.Expression
    , fetchPageResponse : Elm.Expression -> Elm.Expression
    , fetchPage : Elm.Expression -> Elm.Expression
    , sendPages : Elm.Expression -> Elm.Expression
    , initialize : Elm.Expression -> Elm.Expression
    , updateCode : Elm.Expression -> Elm.Expression
    , updatedEditor : Elm.Expression -> Elm.Expression
    }
call_ =
    { blurs =
        \blursArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "blurs"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.unit ] (Type.var "msg") ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ blursArg ]
    , log =
        \logArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "log"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ logArg ]
    , fetchPageResponse =
        \fetchPageResponseArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "fetchPageResponse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.tuple
                                        Type.int
                                        (Type.namedWith [] "Value" [])
                                    ]
                                    (Type.var "msg")
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ fetchPageResponseArg ]
    , fetchPage =
        \fetchPageArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "fetchPage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ fetchPageArg ]
    , sendPages =
        \sendPagesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "sendPages"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        Type.int
                                        (Type.namedWith [] "Value" [])
                                    )
                                ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ sendPagesArg ]
    , initialize =
        \initializeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "initialize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ initializeArg ]
    , updateCode =
        \updateCodeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "updateCode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ updateCodeArg ]
    , updatedEditor =
        \updatedEditorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ports" ]
                    , name = "updatedEditor"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ updatedEditorArg ]
    }


values_ :
    { blurs : Elm.Expression
    , log : Elm.Expression
    , fetchPageResponse : Elm.Expression
    , fetchPage : Elm.Expression
    , sendPages : Elm.Expression
    , initialize : Elm.Expression
    , updateCode : Elm.Expression
    , updatedEditor : Elm.Expression
    }
values_ =
    { blurs =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "blurs"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.unit ] (Type.var "msg") ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , log =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "log"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , fetchPageResponse =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "fetchPageResponse"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.tuple Type.int (Type.namedWith [] "Value" [])
                            ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , fetchPage =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "fetchPage"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , sendPages =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "sendPages"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple Type.int (Type.namedWith [] "Value" []))
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , initialize =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "initialize"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , updateCode =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "updateCode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , updatedEditor =
        Elm.value
            { importFrom = [ "Ports" ]
            , name = "updatedEditor"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    }


