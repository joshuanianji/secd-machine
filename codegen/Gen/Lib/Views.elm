module Gen.Lib.Views exposing (bold, button, call_, link, moduleName_, togglableTitle, toggleButtons, unselectable, values_, viewTogglable)

{-| 
@docs values_, call_, button, link, toggleButtons, unselectable, togglableTitle, viewTogglable, bold, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Lib", "Views" ]


{-| bold: String -> Element msg -}
bold : String -> Elm.Expression
bold boldArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "bold"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.string boldArg ]


{-| viewTogglable: 
    List (Attribute msg)
    -> { title : String, activeWhen : Bool, onClick : msg, body : Element msg }
    -> Element msg
-}
viewTogglable :
    List Elm.Expression
    -> { title : String
    , activeWhen : Bool
    , onClick : Elm.Expression
    , body : Elm.Expression
    }
    -> Elm.Expression
viewTogglable viewTogglableArg viewTogglableArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "viewTogglable"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "title", Type.string )
                            , ( "activeWhen", Type.bool )
                            , ( "onClick", Type.var "msg" )
                            , ( "body"
                              , Type.namedWith [] "Element" [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list viewTogglableArg
        , Elm.record
            [ Tuple.pair "title" (Elm.string viewTogglableArg0.title)
            , Tuple.pair "activeWhen" (Elm.bool viewTogglableArg0.activeWhen)
            , Tuple.pair "onClick" viewTogglableArg0.onClick
            , Tuple.pair "body" viewTogglableArg0.body
            ]
        ]


{-| togglableTitle: 
    List (Attribute msg)
    -> { label : String, activeWhen : Bool, onClick : msg }
    -> Element msg
-}
togglableTitle :
    List Elm.Expression
    -> { label : String, activeWhen : Bool, onClick : Elm.Expression }
    -> Elm.Expression
togglableTitle togglableTitleArg togglableTitleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "togglableTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "label", Type.string )
                            , ( "activeWhen", Type.bool )
                            , ( "onClick", Type.var "msg" )
                            ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list togglableTitleArg
        , Elm.record
            [ Tuple.pair "label" (Elm.string togglableTitleArg0.label)
            , Tuple.pair "activeWhen" (Elm.bool togglableTitleArg0.activeWhen)
            , Tuple.pair "onClick" togglableTitleArg0.onClick
            ]
        ]


{-| unselectable: Attribute msg -}
unselectable : Elm.Expression
unselectable =
    Elm.value
        { importFrom = [ "Lib", "Views" ]
        , name = "unselectable"
        , annotation = Just (Type.namedWith [] "Attribute" [ Type.var "msg" ])
        }


{-| toggleButtons: 
    Bool
    -> List (Attribute msg)
    -> List { active : Bool, onPress : Maybe msg, label : Element msg }
    -> Element msg
-}
toggleButtons :
    Bool
    -> List Elm.Expression
    -> List { active : Bool, onPress : Elm.Expression, label : Elm.Expression }
    -> Elm.Expression
toggleButtons toggleButtonsArg toggleButtonsArg0 toggleButtonsArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "toggleButtons"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.list
                            (Type.record
                                [ ( "active", Type.bool )
                                , ( "onPress"
                                  , Type.namedWith [] "Maybe" [ Type.var "msg" ]
                                  )
                                , ( "label"
                                  , Type.namedWith
                                        []
                                        "Element"
                                        [ Type.var "msg" ]
                                  )
                                ]
                            )
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.bool toggleButtonsArg
        , Elm.list toggleButtonsArg0
        , Elm.list
            (List.map
                (\unpack ->
                    Elm.record
                        [ Tuple.pair "active" (Elm.bool unpack.active)
                        , Tuple.pair "onPress" unpack.onPress
                        , Tuple.pair "label" unpack.label
                        ]
                )
                toggleButtonsArg1
            )
        ]


{-| link: List (Attribute msg) -> { url : String, label : String } -> Element msg -}
link : List Elm.Expression -> { url : String, label : String } -> Elm.Expression
link linkArg linkArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "url", Type.string ), ( "label", Type.string ) ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list linkArg
        , Elm.record
            [ Tuple.pair "url" (Elm.string linkArg0.url)
            , Tuple.pair "label" (Elm.string linkArg0.label)
            ]
        ]


{-| button: msg -> Element msg -> Element msg -}
button : Elm.Expression -> Elm.Expression -> Elm.Expression
button buttonArg buttonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg"
                        , Type.namedWith [] "Element" [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ buttonArg, buttonArg0 ]


call_ :
    { bold : Elm.Expression -> Elm.Expression
    , viewTogglable : Elm.Expression -> Elm.Expression -> Elm.Expression
    , togglableTitle : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toggleButtons :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , link : Elm.Expression -> Elm.Expression -> Elm.Expression
    , button : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { bold =
        \boldArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Views" ]
                    , name = "bold"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ boldArg ]
    , viewTogglable =
        \viewTogglableArg viewTogglableArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Views" ]
                    , name = "viewTogglable"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        []
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "title", Type.string )
                                    , ( "activeWhen", Type.bool )
                                    , ( "onClick", Type.var "msg" )
                                    , ( "body"
                                      , Type.namedWith
                                            []
                                            "Element"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewTogglableArg, viewTogglableArg0 ]
    , togglableTitle =
        \togglableTitleArg togglableTitleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Views" ]
                    , name = "togglableTitle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        []
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "label", Type.string )
                                    , ( "activeWhen", Type.bool )
                                    , ( "onClick", Type.var "msg" )
                                    ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ togglableTitleArg, togglableTitleArg0 ]
    , toggleButtons =
        \toggleButtonsArg toggleButtonsArg0 toggleButtonsArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Views" ]
                    , name = "toggleButtons"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool
                                , Type.list
                                    (Type.namedWith
                                        []
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.record
                                        [ ( "active", Type.bool )
                                        , ( "onPress"
                                          , Type.namedWith
                                                []
                                                "Maybe"
                                                [ Type.var "msg" ]
                                          )
                                        , ( "label"
                                          , Type.namedWith
                                                []
                                                "Element"
                                                [ Type.var "msg" ]
                                          )
                                        ]
                                    )
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ toggleButtonsArg, toggleButtonsArg0, toggleButtonsArg1 ]
    , link =
        \linkArg linkArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Views" ]
                    , name = "link"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        []
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "url", Type.string )
                                    , ( "label", Type.string )
                                    ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ linkArg, linkArg0 ]
    , button =
        \buttonArg buttonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Views" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg"
                                , Type.namedWith [] "Element" [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ buttonArg, buttonArg0 ]
    }


values_ :
    { bold : Elm.Expression
    , viewTogglable : Elm.Expression
    , togglableTitle : Elm.Expression
    , unselectable : Elm.Expression
    , toggleButtons : Elm.Expression
    , link : Elm.Expression
    , button : Elm.Expression
    }
values_ =
    { bold =
        Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "bold"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewTogglable =
        Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "viewTogglable"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "title", Type.string )
                            , ( "activeWhen", Type.bool )
                            , ( "onClick", Type.var "msg" )
                            , ( "body"
                              , Type.namedWith [] "Element" [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , togglableTitle =
        Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "togglableTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "label", Type.string )
                            , ( "activeWhen", Type.bool )
                            , ( "onClick", Type.var "msg" )
                            ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , unselectable =
        Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "unselectable"
            , annotation =
                Just (Type.namedWith [] "Attribute" [ Type.var "msg" ])
            }
    , toggleButtons =
        Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "toggleButtons"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.list
                            (Type.record
                                [ ( "active", Type.bool )
                                , ( "onPress"
                                  , Type.namedWith [] "Maybe" [ Type.var "msg" ]
                                  )
                                , ( "label"
                                  , Type.namedWith
                                        []
                                        "Element"
                                        [ Type.var "msg" ]
                                  )
                                ]
                            )
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , link =
        Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "link"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "url", Type.string ), ( "label", Type.string ) ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , button =
        Elm.value
            { importFrom = [ "Lib", "Views" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg"
                        , Type.namedWith [] "Element" [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    }


