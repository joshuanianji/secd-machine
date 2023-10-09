module Gen.Lib.Util exposing (addIf, call_, classifyDevice, deadEndToString, deadEndsToString, eachZero, eachZeroBorder, foldResult, getLocationInfo, moduleName_, problemToString, runIf, showMaybeInt, surround, values_, viewIcon, wrapAdd, zipperNth)

{-| 
@docs values_, call_, viewIcon, eachZero, eachZeroBorder, wrapAdd, showMaybeInt, foldResult, classifyDevice, addIf, runIf, zipperNth, getLocationInfo, surround, deadEndsToString, deadEndToString, problemToString, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Lib", "Util" ]


{-| problemToString: Problem -> String -}
problemToString : Elm.Expression -> Elm.Expression
problemToString problemToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "problemToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Problem" [] ]
                        Type.string
                    )
            }
        )
        [ problemToStringArg ]


{-| deadEndToString: DeadEnd -> String -}
deadEndToString : Elm.Expression -> Elm.Expression
deadEndToString deadEndToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "deadEndToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "DeadEnd" [] ]
                        Type.string
                    )
            }
        )
        [ deadEndToStringArg ]


{-| deadEndsToString: List DeadEnd -> String -}
deadEndsToString : List Elm.Expression -> Elm.Expression
deadEndsToString deadEndsToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "deadEndsToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "DeadEnd" []) ]
                        Type.string
                    )
            }
        )
        [ Elm.list deadEndsToStringArg ]


{-| surround: 
    List (Attribute msg)
    -> { left : Int, middle : Int, right : Int }
    -> Element msg
    -> Element msg
-}
surround :
    List Elm.Expression
    -> { left : Int, middle : Int, right : Int }
    -> Elm.Expression
    -> Elm.Expression
surround surroundArg surroundArg0 surroundArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "surround"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "left", Type.int )
                            , ( "middle", Type.int )
                            , ( "right", Type.int )
                            ]
                        , Type.namedWith [] "Element" [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list surroundArg
        , Elm.record
            [ Tuple.pair "left" (Elm.int surroundArg0.left)
            , Tuple.pair "middle" (Elm.int surroundArg0.middle)
            , Tuple.pair "right" (Elm.int surroundArg0.right)
            ]
        , surroundArg1
        ]


{-| {-|


## getLocationInfo

takes in an index and finds out which page and chunk it's from. 0-indexed.

Returns:

    { pageNum = Int -- which page to retrieve JS from
    , pageLocation = Int -- where inside the page to load the chunk
    , chunkLocation = Int -- where inside the chunk to go to
    }

-}

getLocationInfo: 
    Int
    -> { a | pageSize : Int, chunkSize : Int }
    -> { pageNum : Int, pageLocation : Int, chunkLocation : Int }
-}
getLocationInfo :
    Int -> { a | pageSize : Int, chunkSize : Int } -> Elm.Expression
getLocationInfo getLocationInfoArg getLocationInfoArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "getLocationInfo"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.extensible
                            "a"
                            [ ( "pageSize", Type.int )
                            , ( "chunkSize", Type.int )
                            ]
                        ]
                        (Type.record
                            [ ( "pageNum", Type.int )
                            , ( "pageLocation", Type.int )
                            , ( "chunkLocation", Type.int )
                            ]
                        )
                    )
            }
        )
        [ Elm.int getLocationInfoArg
        , Elm.record
            [ Tuple.pair "pageSize" (Elm.int getLocationInfoArg0.pageSize)
            , Tuple.pair "chunkSize" (Elm.int getLocationInfoArg0.chunkSize)
            ]
        ]


{-| {-|


## focusN

focus on the nth element in a zipper

If `n<=0`, we focus on the first element

If `n` is out of bounds, the zipper points to the last element

-}

zipperNth: Int -> Zipper a -> Zipper a
-}
zipperNth : Int -> Elm.Expression -> Elm.Expression
zipperNth zipperNthArg zipperNthArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "zipperNth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [] "Zipper" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Zipper" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.int zipperNthArg, zipperNthArg0 ]


{-| {-|


## runIf

conditionally runs a function

-}

runIf: Bool -> (a -> a) -> a -> a
-}
runIf :
    Bool
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
runIf runIfArg runIfArg0 runIfArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "runIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.function [ Type.var "a" ] (Type.var "a")
                        , Type.var "a"
                        ]
                        (Type.var "a")
                    )
            }
        )
        [ Elm.bool runIfArg
        , Elm.functionReduced "runIfUnpack" runIfArg0
        , runIfArg1
        ]


{-| {-|


## addIf

conditionally prepends a list

-}

addIf: Bool -> List a -> List a -> List a
-}
addIf : Bool -> List Elm.Expression -> List Elm.Expression -> Elm.Expression
addIf addIfArg addIfArg0 addIfArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "addIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.list (Type.var "a")
                        , Type.list (Type.var "a")
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
        )
        [ Elm.bool addIfArg, Elm.list addIfArg0, Elm.list addIfArg1 ]


{-| classifyDevice: Int -> Element.DeviceClass -}
classifyDevice : Int -> Elm.Expression
classifyDevice classifyDeviceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "classifyDevice"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element" ] "DeviceClass" [])
                    )
            }
        )
        [ Elm.int classifyDeviceArg ]


{-| foldResult: Result a a -> a -}
foldResult : Elm.Expression -> Elm.Expression
foldResult foldResultArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "foldResult"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            []
                            "Result"
                            [ Type.var "a", Type.var "a" ]
                        ]
                        (Type.var "a")
                    )
            }
        )
        [ foldResultArg ]


{-| showMaybeInt: Maybe Int -> String -}
showMaybeInt : Elm.Expression -> Elm.Expression
showMaybeInt showMaybeIntArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "showMaybeInt"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ] ]
                        Type.string
                    )
            }
        )
        [ showMaybeIntArg ]


{-| wrapAdd: a -> a -> List a -> List a -}
wrapAdd :
    Elm.Expression -> Elm.Expression -> List Elm.Expression -> Elm.Expression
wrapAdd wrapAddArg wrapAddArg0 wrapAddArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "wrapAdd"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a", Type.var "a", Type.list (Type.var "a") ]
                        (Type.list (Type.var "a"))
                    )
            }
        )
        [ wrapAddArg, wrapAddArg0, Elm.list wrapAddArg1 ]


{-| eachZeroBorder: { topLeft : Int, topRight : Int, bottomRight : Int, bottomLeft : Int } -}
eachZeroBorder : Elm.Expression
eachZeroBorder =
    Elm.value
        { importFrom = [ "Lib", "Util" ]
        , name = "eachZeroBorder"
        , annotation =
            Just
                (Type.record
                    [ ( "topLeft", Type.int )
                    , ( "topRight", Type.int )
                    , ( "bottomRight", Type.int )
                    , ( "bottomLeft", Type.int )
                    ]
                )
        }


{-| eachZero: { top : Int, right : Int, bottom : Int, left : Int } -}
eachZero : Elm.Expression
eachZero =
    Elm.value
        { importFrom = [ "Lib", "Util" ]
        , name = "eachZero"
        , annotation =
            Just
                (Type.record
                    [ ( "top", Type.int )
                    , ( "right", Type.int )
                    , ( "bottom", Type.int )
                    , ( "left", Type.int )
                    ]
                )
        }


{-| viewIcon: List (Attribute msg) -> Icon -> Element msg -}
viewIcon : List Elm.Expression -> Elm.Expression -> Elm.Expression
viewIcon viewIconArg viewIconArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "viewIcon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.namedWith [] "Icon" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list viewIconArg, viewIconArg0 ]


call_ :
    { problemToString : Elm.Expression -> Elm.Expression
    , deadEndToString : Elm.Expression -> Elm.Expression
    , deadEndsToString : Elm.Expression -> Elm.Expression
    , surround :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , getLocationInfo : Elm.Expression -> Elm.Expression -> Elm.Expression
    , zipperNth : Elm.Expression -> Elm.Expression -> Elm.Expression
    , runIf :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , addIf :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , classifyDevice : Elm.Expression -> Elm.Expression
    , foldResult : Elm.Expression -> Elm.Expression
    , showMaybeInt : Elm.Expression -> Elm.Expression
    , wrapAdd :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewIcon : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { problemToString =
        \problemToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "problemToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Problem" [] ]
                                Type.string
                            )
                    }
                )
                [ problemToStringArg ]
    , deadEndToString =
        \deadEndToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "deadEndToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "DeadEnd" [] ]
                                Type.string
                            )
                    }
                )
                [ deadEndToStringArg ]
    , deadEndsToString =
        \deadEndsToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "deadEndsToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.namedWith [] "DeadEnd" []) ]
                                Type.string
                            )
                    }
                )
                [ deadEndsToStringArg ]
    , surround =
        \surroundArg surroundArg0 surroundArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "surround"
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
                                    [ ( "left", Type.int )
                                    , ( "middle", Type.int )
                                    , ( "right", Type.int )
                                    ]
                                , Type.namedWith [] "Element" [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ surroundArg, surroundArg0, surroundArg1 ]
    , getLocationInfo =
        \getLocationInfoArg getLocationInfoArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "getLocationInfo"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.extensible
                                    "a"
                                    [ ( "pageSize", Type.int )
                                    , ( "chunkSize", Type.int )
                                    ]
                                ]
                                (Type.record
                                    [ ( "pageNum", Type.int )
                                    , ( "pageLocation", Type.int )
                                    , ( "chunkLocation", Type.int )
                                    ]
                                )
                            )
                    }
                )
                [ getLocationInfoArg, getLocationInfoArg0 ]
    , zipperNth =
        \zipperNthArg zipperNthArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "zipperNth"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith [] "Zipper" [ Type.var "a" ]
                                ]
                                (Type.namedWith [] "Zipper" [ Type.var "a" ])
                            )
                    }
                )
                [ zipperNthArg, zipperNthArg0 ]
    , runIf =
        \runIfArg runIfArg0 runIfArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "runIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool
                                , Type.function [ Type.var "a" ] (Type.var "a")
                                , Type.var "a"
                                ]
                                (Type.var "a")
                            )
                    }
                )
                [ runIfArg, runIfArg0, runIfArg1 ]
    , addIf =
        \addIfArg addIfArg0 addIfArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "addIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool
                                , Type.list (Type.var "a")
                                , Type.list (Type.var "a")
                                ]
                                (Type.list (Type.var "a"))
                            )
                    }
                )
                [ addIfArg, addIfArg0, addIfArg1 ]
    , classifyDevice =
        \classifyDeviceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "classifyDevice"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [ "Element" ] "DeviceClass" [])
                            )
                    }
                )
                [ classifyDeviceArg ]
    , foldResult =
        \foldResultArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "foldResult"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    []
                                    "Result"
                                    [ Type.var "a", Type.var "a" ]
                                ]
                                (Type.var "a")
                            )
                    }
                )
                [ foldResultArg ]
    , showMaybeInt =
        \showMaybeIntArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "showMaybeInt"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ] ]
                                Type.string
                            )
                    }
                )
                [ showMaybeIntArg ]
    , wrapAdd =
        \wrapAddArg wrapAddArg0 wrapAddArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "wrapAdd"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a"
                                , Type.var "a"
                                , Type.list (Type.var "a")
                                ]
                                (Type.list (Type.var "a"))
                            )
                    }
                )
                [ wrapAddArg, wrapAddArg0, wrapAddArg1 ]
    , viewIcon =
        \viewIconArg viewIconArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Lib", "Util" ]
                    , name = "viewIcon"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        []
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith [] "Icon" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewIconArg, viewIconArg0 ]
    }


values_ :
    { problemToString : Elm.Expression
    , deadEndToString : Elm.Expression
    , deadEndsToString : Elm.Expression
    , surround : Elm.Expression
    , getLocationInfo : Elm.Expression
    , zipperNth : Elm.Expression
    , runIf : Elm.Expression
    , addIf : Elm.Expression
    , classifyDevice : Elm.Expression
    , foldResult : Elm.Expression
    , showMaybeInt : Elm.Expression
    , wrapAdd : Elm.Expression
    , eachZeroBorder : Elm.Expression
    , eachZero : Elm.Expression
    , viewIcon : Elm.Expression
    }
values_ =
    { problemToString =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "problemToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Problem" [] ]
                        Type.string
                    )
            }
    , deadEndToString =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "deadEndToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "DeadEnd" [] ]
                        Type.string
                    )
            }
    , deadEndsToString =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "deadEndsToString"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "DeadEnd" []) ]
                        Type.string
                    )
            }
    , surround =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "surround"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.record
                            [ ( "left", Type.int )
                            , ( "middle", Type.int )
                            , ( "right", Type.int )
                            ]
                        , Type.namedWith [] "Element" [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , getLocationInfo =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "getLocationInfo"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.extensible
                            "a"
                            [ ( "pageSize", Type.int )
                            , ( "chunkSize", Type.int )
                            ]
                        ]
                        (Type.record
                            [ ( "pageNum", Type.int )
                            , ( "pageLocation", Type.int )
                            , ( "chunkLocation", Type.int )
                            ]
                        )
                    )
            }
    , zipperNth =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "zipperNth"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [] "Zipper" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Zipper" [ Type.var "a" ])
                    )
            }
    , runIf =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "runIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.function [ Type.var "a" ] (Type.var "a")
                        , Type.var "a"
                        ]
                        (Type.var "a")
                    )
            }
    , addIf =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "addIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.list (Type.var "a")
                        , Type.list (Type.var "a")
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
    , classifyDevice =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "classifyDevice"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element" ] "DeviceClass" [])
                    )
            }
    , foldResult =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "foldResult"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            []
                            "Result"
                            [ Type.var "a", Type.var "a" ]
                        ]
                        (Type.var "a")
                    )
            }
    , showMaybeInt =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "showMaybeInt"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ] ]
                        Type.string
                    )
            }
    , wrapAdd =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "wrapAdd"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a", Type.var "a", Type.list (Type.var "a") ]
                        (Type.list (Type.var "a"))
                    )
            }
    , eachZeroBorder =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "eachZeroBorder"
            , annotation =
                Just
                    (Type.record
                        [ ( "topLeft", Type.int )
                        , ( "topRight", Type.int )
                        , ( "bottomRight", Type.int )
                        , ( "bottomLeft", Type.int )
                        ]
                    )
            }
    , eachZero =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "eachZero"
            , annotation =
                Just
                    (Type.record
                        [ ( "top", Type.int )
                        , ( "right", Type.int )
                        , ( "bottom", Type.int )
                        , ( "left", Type.int )
                        ]
                    )
            }
    , viewIcon =
        Elm.value
            { importFrom = [ "Lib", "Util" ]
            , name = "viewIcon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        , Type.namedWith [] "Icon" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    }


