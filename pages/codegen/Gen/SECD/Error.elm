module Gen.SECD.Error exposing (annotation_, call_, moduleName_, values_, view)

{-| 
@docs values_, call_, annotation_, view, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "SECD", "Error" ]


{-| view: Error -> Html msg -}
view : Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Error" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Error" [] ]
                        (Type.namedWith [] "Html" [ Type.var "msg" ])
                    )
            }
        )
        [ viewArg ]


annotation_ : { error : Type.Annotation }
annotation_ =
    { error = Type.alias moduleName_ "Error" [] Type.string }


call_ : { view : Elm.Expression -> Elm.Expression }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Error" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Error" [] ]
                                (Type.namedWith [] "Html" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewArg ]
    }


values_ : { view : Elm.Expression }
values_ =
    { view =
        Elm.value
            { importFrom = [ "SECD", "Error" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Error" [] ]
                        (Type.namedWith [] "Html" [ Type.var "msg" ])
                    )
            }
    }


