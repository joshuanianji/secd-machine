module Gen.Lib.Colours exposing (black, call_, darkgrey, grey, greyAlpha, lightGrey, linkBlue, linkIndigo, linkLightBlue, moduleName_, orange, purple, red, slateGrey, transparent, values_, white)

{-| 
@docs values_, call_, greyAlpha, slateGrey, lightGrey, grey, transparent, purple, orange, red, darkgrey, linkBlue, linkLightBlue, linkIndigo, black, white, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Lib", "Colours" ]


{-| white: Color -}
white : Elm.Expression
white =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "white"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| black: Color -}
black : Elm.Expression
black =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "black"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| linkIndigo: Color -}
linkIndigo : Elm.Expression
linkIndigo =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "linkIndigo"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| linkLightBlue: Color -}
linkLightBlue : Elm.Expression
linkLightBlue =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "linkLightBlue"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| linkBlue: Color -}
linkBlue : Elm.Expression
linkBlue =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "linkBlue"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| darkgrey: Color -}
darkgrey : Elm.Expression
darkgrey =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "darkgrey"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| red: Color -}
red : Elm.Expression
red =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "red"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| orange: Color -}
orange : Elm.Expression
orange =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "orange"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| purple: Color -}
purple : Elm.Expression
purple =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "purple"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| transparent: Color -}
transparent : Elm.Expression
transparent =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "transparent"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| grey: Color -}
grey : Elm.Expression
grey =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "grey"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| lightGrey: Color -}
lightGrey : Elm.Expression
lightGrey =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "lightGrey"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| slateGrey: Color -}
slateGrey : Elm.Expression
slateGrey =
    Elm.value
        { importFrom = [ "Lib", "Colours" ]
        , name = "slateGrey"
        , annotation = Just (Type.namedWith [] "Color" [])
        }


{-| {-| The higher the alpha, the less transparent the colour is.
-}

greyAlpha: Float -> Color
-}
greyAlpha : Float -> Elm.Expression
greyAlpha greyAlphaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "greyAlpha"
            , annotation =
                Just
                    (Type.function [ Type.float ] (Type.namedWith [] "Color" [])
                    )
            }
        )
        [ Elm.float greyAlphaArg ]


call_ : { greyAlpha : Elm.Expression -> Elm.Expression }
call_ =
    { greyAlpha =
        \greyAlphaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Colours" ]
                    , name = "greyAlpha"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith [] "Color" [])
                            )
                    }
                )
                [ greyAlphaArg ]
    }


values_ :
    { white : Elm.Expression
    , black : Elm.Expression
    , linkIndigo : Elm.Expression
    , linkLightBlue : Elm.Expression
    , linkBlue : Elm.Expression
    , darkgrey : Elm.Expression
    , red : Elm.Expression
    , orange : Elm.Expression
    , purple : Elm.Expression
    , transparent : Elm.Expression
    , grey : Elm.Expression
    , lightGrey : Elm.Expression
    , slateGrey : Elm.Expression
    , greyAlpha : Elm.Expression
    }
values_ =
    { white =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "white"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , black =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "black"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , linkIndigo =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "linkIndigo"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , linkLightBlue =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "linkLightBlue"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , linkBlue =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "linkBlue"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , darkgrey =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "darkgrey"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , red =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "red"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , orange =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "orange"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , purple =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "purple"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , transparent =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "transparent"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , grey =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "grey"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , lightGrey =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "lightGrey"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , slateGrey =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "slateGrey"
            , annotation = Just (Type.namedWith [] "Color" [])
            }
    , greyAlpha =
        Elm.value
            { importFrom = [ "Lib", "Colours" ]
            , name = "greyAlpha"
            , annotation =
                Just
                    (Type.function [ Type.float ] (Type.namedWith [] "Color" [])
                    )
            }
    }


