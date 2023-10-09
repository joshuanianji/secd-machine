module Gen.Views.Compiled exposing (annotation_, call_, caseOf_, getIndices, init, make_, moduleName_, stripIndices, subscriptions, transpile, update, values_, view)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, init, transpile, stripIndices, getIndices, update, view, subscriptions, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Views", "Compiled" ]


{-| subscriptions: Model -> Sub Msg -}
subscriptions : Elm.Expression -> Elm.Expression
subscriptions subscriptionsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "Compiled" ]
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
            { importFrom = [ "Views", "Compiled" ]
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
            { importFrom = [ "Views", "Compiled" ]
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


{-| getIndices: List Indexed -> List Int -}
getIndices : List Elm.Expression -> Elm.Expression
getIndices getIndicesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "getIndices"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Indexed" []) ]
                        (Type.list Type.int)
                    )
            }
        )
        [ Elm.list getIndicesArg ]


{-| stripIndices: Indexed -> Unindexed -}
stripIndices : Elm.Expression -> Elm.Expression
stripIndices stripIndicesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "stripIndices"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Indexed" [] ]
                        (Type.namedWith [] "Unindexed" [])
                    )
            }
        )
        [ stripIndicesArg ]


{-| transpile: List Prog.Op -> Result Error (List Indexed) -}
transpile : List Elm.Expression -> Elm.Expression
transpile transpileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "transpile"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Prog" ] "Op" []) ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Indexed" [])
                            ]
                        )
                    )
            }
        )
        [ Elm.list transpileArg ]


{-| init: Program -> ( Model, Cmd Msg ) -}
init : Elm.Expression -> Elm.Expression
init initArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
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
        [ initArg ]


annotation_ :
    { model : Type.Annotation
    , msg : Type.Annotation
    , aPType : Type.Annotation
    , code : Type.Annotation -> Type.Annotation
    , unindexed : Type.Annotation
    , indexed : Type.Annotation
    }
annotation_ =
    { model =
        Type.alias
            moduleName_
            "Model"
            []
            (Type.record
                [ ( "original", Type.list (Type.namedWith [ "Prog" ] "Op" []) )
                , ( "transpiled"
                  , Type.namedWith
                        []
                        "Result"
                        [ Type.namedWith [] "Error" []
                        , Type.namedWith [] "OkModel" []
                        ]
                  )
                ]
            )
    , msg = Type.namedWith [ "Views", "Compiled" ] "Msg" []
    , aPType = Type.namedWith [ "Views", "Compiled" ] "APType" []
    , code =
        \codeArg0 -> Type.namedWith [ "Views", "Compiled" ] "Code" [ codeArg0 ]
    , unindexed = Type.namedWith [ "Views", "Compiled" ] "Unindexed" []
    , indexed = Type.namedWith [ "Views", "Compiled" ] "Indexed" []
    }


make_ :
    { model :
        { original : Elm.Expression, transpiled : Elm.Expression }
        -> Elm.Expression
    , aP : Elm.Expression
    , rAP : Elm.Expression
    , nIL : Elm.Expression
    , lD : Elm.Expression -> Elm.Expression
    , lDC : Elm.Expression -> Elm.Expression
    , lDFunc : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lDLambda : Elm.Expression -> Elm.Expression
    , lDApply : Elm.Expression -> Elm.Expression -> Elm.Expression
    , loneAP : Elm.Expression
    , rTN : Elm.Expression
    , sEL : Elm.Expression -> Elm.Expression -> Elm.Expression
    , jOIN : Elm.Expression
    , dUM : Elm.Expression
    , fUNC : Elm.Expression -> Elm.Expression
    , unindexed : Elm.Expression -> Elm.Expression
    , indexed : Elm.Expression -> Elm.Expression
    }
make_ =
    { model =
        \model_args ->
            Elm.withType
                (Type.alias
                    [ "Views", "Compiled" ]
                    "Model"
                    []
                    (Type.record
                        [ ( "original"
                          , Type.list (Type.namedWith [ "Prog" ] "Op" [])
                          )
                        , ( "transpiled"
                          , Type.namedWith
                                []
                                "Result"
                                [ Type.namedWith [] "Error" []
                                , Type.namedWith [] "OkModel" []
                                ]
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "original" model_args.original
                    , Tuple.pair "transpiled" model_args.transpiled
                    ]
                )
    , aP =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "AP"
            , annotation = Just (Type.namedWith [] "APType" [])
            }
    , rAP =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "RAP"
            , annotation = Just (Type.namedWith [] "APType" [])
            }
    , nIL =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "NIL"
            , annotation = Just (Type.namedWith [] "Code" [ Type.var "a" ])
            }
    , lD =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "LD"
                    , annotation =
                        Just (Type.namedWith [] "Code" [ Type.var "a" ])
                    }
                )
                [ ar0 ]
    , lDC =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "LDC"
                    , annotation =
                        Just (Type.namedWith [] "Code" [ Type.var "a" ])
                    }
                )
                [ ar0 ]
    , lDFunc =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "LDFunc"
                    , annotation =
                        Just (Type.namedWith [] "Code" [ Type.var "a" ])
                    }
                )
                [ ar0, ar1 ]
    , lDLambda =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "LDLambda"
                    , annotation =
                        Just (Type.namedWith [] "Code" [ Type.var "a" ])
                    }
                )
                [ ar0 ]
    , lDApply =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "LDApply"
                    , annotation =
                        Just (Type.namedWith [] "Code" [ Type.var "a" ])
                    }
                )
                [ ar0, ar1 ]
    , loneAP =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "LoneAP"
            , annotation = Just (Type.namedWith [] "Code" [ Type.var "a" ])
            }
    , rTN =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "RTN"
            , annotation = Just (Type.namedWith [] "Code" [ Type.var "a" ])
            }
    , sEL =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "SEL"
                    , annotation =
                        Just (Type.namedWith [] "Code" [ Type.var "a" ])
                    }
                )
                [ ar0, ar1 ]
    , jOIN =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "JOIN"
            , annotation = Just (Type.namedWith [] "Code" [ Type.var "a" ])
            }
    , dUM =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "DUM"
            , annotation = Just (Type.namedWith [] "Code" [ Type.var "a" ])
            }
    , fUNC =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "FUNC"
                    , annotation =
                        Just (Type.namedWith [] "Code" [ Type.var "a" ])
                    }
                )
                [ ar0 ]
    , unindexed =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "Unindexed"
                    , annotation = Just (Type.namedWith [] "Unindexed" [])
                    }
                )
                [ ar0 ]
    , indexed =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "Indexed"
                    , annotation = Just (Type.namedWith [] "Indexed" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { aPType :
        Elm.Expression
        -> { aPTypeTags_0_0 | aP : Elm.Expression, rAP : Elm.Expression }
        -> Elm.Expression
    , code :
        Elm.Expression
        -> { codeTags_1_0
            | nIL : Elm.Expression
            , lD : Elm.Expression -> Elm.Expression
            , lDC : Elm.Expression -> Elm.Expression
            , lDFunc : Elm.Expression -> Elm.Expression -> Elm.Expression
            , lDLambda : Elm.Expression -> Elm.Expression
            , lDApply : Elm.Expression -> Elm.Expression -> Elm.Expression
            , loneAP : Elm.Expression
            , rTN : Elm.Expression
            , sEL : Elm.Expression -> Elm.Expression -> Elm.Expression
            , jOIN : Elm.Expression
            , dUM : Elm.Expression
            , fUNC : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    , unindexed :
        Elm.Expression
        -> { unindexedTags_2_0 | unindexed : Elm.Expression -> Elm.Expression }
        -> Elm.Expression
    , indexed :
        Elm.Expression
        -> { indexedTags_3_0 | indexed : Elm.Expression -> Elm.Expression }
        -> Elm.Expression
    }
caseOf_ =
    { aPType =
        \aPTypeExpression aPTypeTags ->
            Elm.Case.custom
                aPTypeExpression
                (Type.namedWith [ "Views", "Compiled" ] "APType" [])
                [ Elm.Case.branch0 "AP" aPTypeTags.aP
                , Elm.Case.branch0 "RAP" aPTypeTags.rAP
                ]
    , code =
        \codeExpression codeTags ->
            Elm.Case.custom
                codeExpression
                (Type.namedWith [ "Views", "Compiled" ] "Code" [ Type.var "a" ])
                [ Elm.Case.branch0 "NIL" codeTags.nIL
                , Elm.Case.branch1
                    "LD"
                    ( "one", Type.tuple Type.int Type.int )
                    codeTags.lD
                , Elm.Case.branch1 "LDC" ( "basics.Int", Type.int ) codeTags.lDC
                , Elm.Case.branch2
                    "LDFunc"
                    ( "string.String", Type.string )
                    ( "list.List", Type.list (Type.var "a") )
                    codeTags.lDFunc
                , Elm.Case.branch1
                    "LDLambda"
                    ( "list.List", Type.list (Type.var "a") )
                    codeTags.lDLambda
                , Elm.Case.branch2
                    "LDApply"
                    ( "aPType", Type.namedWith [] "APType" [] )
                    ( "list.List", Type.list (Type.var "a") )
                    codeTags.lDApply
                , Elm.Case.branch0 "LoneAP" codeTags.loneAP
                , Elm.Case.branch0 "RTN" codeTags.rTN
                , Elm.Case.branch2
                    "SEL"
                    ( "list.List", Type.list (Type.var "a") )
                    ( "list.List", Type.list (Type.var "a") )
                    codeTags.sEL
                , Elm.Case.branch0 "JOIN" codeTags.jOIN
                , Elm.Case.branch0 "DUM" codeTags.dUM
                , Elm.Case.branch1
                    "FUNC"
                    ( "string.String", Type.string )
                    codeTags.fUNC
                ]
    , unindexed =
        \unindexedExpression unindexedTags ->
            Elm.Case.custom
                unindexedExpression
                (Type.namedWith [ "Views", "Compiled" ] "Unindexed" [])
                [ Elm.Case.branch1
                    "Unindexed"
                    ( "code"
                    , Type.namedWith
                        []
                        "Code"
                        [ Type.namedWith [] "Unindexed" [] ]
                    )
                    unindexedTags.unindexed
                ]
    , indexed =
        \indexedExpression indexedTags ->
            Elm.Case.custom
                indexedExpression
                (Type.namedWith [ "Views", "Compiled" ] "Indexed" [])
                [ Elm.Case.branch1
                    "Indexed"
                    ( "one"
                    , Type.tuple
                        Type.int
                        (Type.namedWith
                            []
                            "Code"
                            [ Type.namedWith [] "Indexed" [] ]
                        )
                    )
                    indexedTags.indexed
                ]
    }


call_ :
    { subscriptions : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression -> Elm.Expression
    , getIndices : Elm.Expression -> Elm.Expression
    , stripIndices : Elm.Expression -> Elm.Expression
    , transpile : Elm.Expression -> Elm.Expression
    , init : Elm.Expression -> Elm.Expression
    }
call_ =
    { subscriptions =
        \subscriptionsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
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
                    { importFrom = [ "Views", "Compiled" ]
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
                    { importFrom = [ "Views", "Compiled" ]
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
    , getIndices =
        \getIndicesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "getIndices"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.namedWith [] "Indexed" []) ]
                                (Type.list Type.int)
                            )
                    }
                )
                [ getIndicesArg ]
    , stripIndices =
        \stripIndicesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "stripIndices"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Indexed" [] ]
                                (Type.namedWith [] "Unindexed" [])
                            )
                    }
                )
                [ stripIndicesArg ]
    , transpile =
        \transpileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "transpile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.namedWith [ "Prog" ] "Op" [])
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Indexed" [])
                                    ]
                                )
                            )
                    }
                )
                [ transpileArg ]
    , init =
        \initArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Views", "Compiled" ]
                    , name = "init"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Program" [] ]
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
                [ initArg ]
    }


values_ :
    { subscriptions : Elm.Expression
    , view : Elm.Expression
    , update : Elm.Expression
    , getIndices : Elm.Expression
    , stripIndices : Elm.Expression
    , transpile : Elm.Expression
    , init : Elm.Expression
    }
values_ =
    { subscriptions =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
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
            { importFrom = [ "Views", "Compiled" ]
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
            { importFrom = [ "Views", "Compiled" ]
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
    , getIndices =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "getIndices"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Indexed" []) ]
                        (Type.list Type.int)
                    )
            }
    , stripIndices =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "stripIndices"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Indexed" [] ]
                        (Type.namedWith [] "Unindexed" [])
                    )
            }
    , transpile =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "transpile"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Prog" ] "Op" []) ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Indexed" [])
                            ]
                        )
                    )
            }
    , init =
        Elm.value
            { importFrom = [ "Views", "Compiled" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
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


