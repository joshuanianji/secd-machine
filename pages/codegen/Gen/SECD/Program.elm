module Gen.SECD.Program exposing (addVarNames, annotation_, call_, caseOf_, cmpFunc, cmpToString, compile, compileArgs, compileCons, compileFunc, compileFuncApp, compileLambda, compileLet, compileLetBinding, compileLetHelper, compileLetrec, compile_, decoder, emptyEnv, encode, encodeCompare, encodeFunc, encodeSingle, fromList, fromSingleton, funcToString, lookup, make_, moduleName_, opToString, singleDecoder, toList, values_, view)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, cmpFunc, opToString, funcToString, cmpToString, view, toList, fromList, fromSingleton, compile, compile_, compileCons, compileFuncApp, compileLambda, compileLet, compileLetrec, compileLetHelper, compileLetBinding, compileFunc, compileArgs, emptyEnv, lookup, addVarNames, encode, decoder, encodeSingle, encodeFunc, encodeCompare, singleDecoder, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "SECD", "Program" ]


{-| singleDecoder: Decoder Op -}
singleDecoder : Elm.Expression
singleDecoder =
    Elm.value
        { importFrom = [ "SECD", "Program" ]
        , name = "singleDecoder"
        , annotation =
            Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "Op" [] ])
        }


{-| encodeCompare: Cmp -> Value -}
encodeCompare : Elm.Expression -> Elm.Expression
encodeCompare encodeCompareArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encodeCompare"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cmp" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
        )
        [ encodeCompareArg ]


{-| encodeFunc: Func -> Value -}
encodeFunc : Elm.Expression -> Elm.Expression
encodeFunc encodeFuncArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encodeFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Func" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
        )
        [ encodeFuncArg ]


{-| encodeSingle: Op -> Value -}
encodeSingle : Elm.Expression -> Elm.Expression
encodeSingle encodeSingleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encodeSingle"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Op" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
        )
        [ encodeSingleArg ]


{-| decoder: Decoder Program -}
decoder : Elm.Expression
decoder =
    Elm.value
        { importFrom = [ "SECD", "Program" ]
        , name = "decoder"
        , annotation =
            Just
                (Type.namedWith [] "Decoder" [ Type.namedWith [] "Program" [] ])
        }


{-| encode: Program -> Value -}
encode : Elm.Expression -> Elm.Expression
encode encodeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
        )
        [ encodeArg ]


{-| addVarNames: List String -> Environment -> Environment -}
addVarNames : List String -> Elm.Expression -> Elm.Expression
addVarNames addVarNamesArg addVarNamesArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "addVarNames"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string
                        , Type.namedWith [] "Environment" []
                        ]
                        (Type.namedWith [] "Environment" [])
                    )
            }
        )
        [ Elm.list (List.map Elm.string addVarNamesArg), addVarNamesArg0 ]


{-| lookup: String -> Environment -> Result Error Op -}
lookup : String -> Elm.Expression -> Elm.Expression
lookup lookupArg lookupArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "lookup"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.namedWith [] "Environment" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.namedWith [] "Op" []
                            ]
                        )
                    )
            }
        )
        [ Elm.string lookupArg, lookupArg0 ]


{-| emptyEnv: Environment -}
emptyEnv : Elm.Expression
emptyEnv =
    Elm.value
        { importFrom = [ "SECD", "Program" ]
        , name = "emptyEnv"
        , annotation = Just (Type.namedWith [] "Environment" [])
        }


{-| compileArgs: Environment -> Bool -> List AST -> Result Error (List Op) -}
compileArgs : Elm.Expression -> Bool -> List Elm.Expression -> Elm.Expression
compileArgs compileArgsArg compileArgsArg0 compileArgsArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileArgs"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.bool
                        , Type.list (Type.namedWith [] "AST" [])
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ compileArgsArg, Elm.bool compileArgsArg0, Elm.list compileArgsArg1 ]


{-| compileFunc: Environment -> AST -> Result Error ( Maybe Int, List Op, Bool ) -}
compileFunc : Elm.Expression -> Elm.Expression -> Elm.Expression
compileFunc compileFuncArg compileFuncArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.triple
                                (Type.namedWith [] "Maybe" [ Type.int ])
                                (Type.list (Type.namedWith [] "Op" []))
                                Type.bool
                            ]
                        )
                    )
            }
        )
        [ compileFuncArg, compileFuncArg0 ]


{-| compileLetBinding: Environment -> ( Token, AST ) -> Result Error (List Op) -}
compileLetBinding : Elm.Expression -> Elm.Expression -> Elm.Expression
compileLetBinding compileLetBindingArg compileLetBindingArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLetBinding"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.tuple
                            (Type.namedWith [] "Token" [])
                            (Type.namedWith [] "AST" [])
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ compileLetBindingArg, compileLetBindingArg0 ]


{-| compileLetHelper: Bool -> Environment -> List ( Token, AST ) -> AST -> Result Error (List Op) -}
compileLetHelper :
    Bool
    -> Elm.Expression
    -> List Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
compileLetHelper compileLetHelperArg compileLetHelperArg0 compileLetHelperArg1 compileLetHelperArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLetHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.namedWith [] "Environment" []
                        , Type.list
                            (Type.tuple
                                (Type.namedWith [] "Token" [])
                                (Type.namedWith [] "AST" [])
                            )
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ Elm.bool compileLetHelperArg
        , compileLetHelperArg0
        , Elm.list compileLetHelperArg1
        , compileLetHelperArg2
        ]


{-| compileLetrec: Environment -> List ( Token, AST ) -> AST -> Result Error (List Op) -}
compileLetrec :
    Elm.Expression -> List Elm.Expression -> Elm.Expression -> Elm.Expression
compileLetrec compileLetrecArg compileLetrecArg0 compileLetrecArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLetrec"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.list
                            (Type.tuple
                                (Type.namedWith [] "Token" [])
                                (Type.namedWith [] "AST" [])
                            )
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ compileLetrecArg, Elm.list compileLetrecArg0, compileLetrecArg1 ]


{-| compileLet: Environment -> List ( Token, AST ) -> AST -> Result Error (List Op) -}
compileLet :
    Elm.Expression -> List Elm.Expression -> Elm.Expression -> Elm.Expression
compileLet compileLetArg compileLetArg0 compileLetArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLet"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.list
                            (Type.tuple
                                (Type.namedWith [] "Token" [])
                                (Type.namedWith [] "AST" [])
                            )
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ compileLetArg, Elm.list compileLetArg0, compileLetArg1 ]


{-| compileLambda: Maybe String -> Environment -> List Token -> AST -> Result Error (List Op) -}
compileLambda :
    Elm.Expression
    -> Elm.Expression
    -> List Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
compileLambda compileLambdaArg compileLambdaArg0 compileLambdaArg1 compileLambdaArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLambda"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.string ]
                        , Type.namedWith [] "Environment" []
                        , Type.list (Type.namedWith [] "Token" [])
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ compileLambdaArg
        , compileLambdaArg0
        , Elm.list compileLambdaArg1
        , compileLambdaArg2
        ]


{-| compileFuncApp: Environment -> AST -> List AST -> Bool -> Result Error (List Op) -}
compileFuncApp :
    Elm.Expression
    -> Elm.Expression
    -> List Elm.Expression
    -> Bool
    -> Elm.Expression
compileFuncApp compileFuncAppArg compileFuncAppArg0 compileFuncAppArg1 compileFuncAppArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileFuncApp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.namedWith [] "AST" []
                        , Type.list (Type.namedWith [] "AST" [])
                        , Type.bool
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ compileFuncAppArg
        , compileFuncAppArg0
        , Elm.list compileFuncAppArg1
        , Elm.bool compileFuncAppArg2
        ]


{-| compileCons: Cons Int -> List Op -}
compileCons : Elm.Expression -> Elm.Expression
compileCons compileConsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileCons"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cons" [ Type.int ] ]
                        (Type.list (Type.namedWith [] "Op" []))
                    )
            }
        )
        [ compileConsArg ]


{-| compile_: Environment -> AST -> Result Error (List Op) -}
compile_ : Elm.Expression -> Elm.Expression -> Elm.Expression
compile_ compile_Arg compile_Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compile_"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
        )
        [ compile_Arg, compile_Arg0 ]


{-| compile: AST -> Result Error Program -}
compile : Elm.Expression -> Elm.Expression
compile compileArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "AST" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.namedWith [] "Program" []
                            ]
                        )
                    )
            }
        )
        [ compileArg ]


{-| fromSingleton: Op -> Program -}
fromSingleton : Elm.Expression -> Elm.Expression
fromSingleton fromSingletonArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "fromSingleton"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Op" [] ]
                        (Type.namedWith [] "Program" [])
                    )
            }
        )
        [ fromSingletonArg ]


{-| fromList: List Op -> Program -}
fromList : List Elm.Expression -> Elm.Expression
fromList fromListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "fromList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Op" []) ]
                        (Type.namedWith [] "Program" [])
                    )
            }
        )
        [ Elm.list fromListArg ]


{-| toList: Program -> List Op -}
toList : Elm.Expression -> Elm.Expression
toList toListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "toList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.list (Type.namedWith [] "Op" []))
                    )
            }
        )
        [ toListArg ]


{-| view: Op -> List (Element msg) -}
view : Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Op" [] ]
                        (Type.list
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        )
                    )
            }
        )
        [ viewArg ]


{-| cmpToString: Cmp -> String -}
cmpToString : Elm.Expression -> Elm.Expression
cmpToString cmpToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "cmpToString"
            , annotation =
                Just (Type.function [ Type.namedWith [] "Cmp" [] ] Type.string)
            }
        )
        [ cmpToStringArg ]


{-| funcToString: Func -> String -}
funcToString : Elm.Expression -> Elm.Expression
funcToString funcToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "funcToString"
            , annotation =
                Just (Type.function [ Type.namedWith [] "Func" [] ] Type.string)
            }
        )
        [ funcToStringArg ]


{-| opToString: Op -> String -}
opToString : Elm.Expression -> Elm.Expression
opToString opToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "opToString"
            , annotation =
                Just (Type.function [ Type.namedWith [] "Op" [] ] Type.string)
            }
        )
        [ opToStringArg ]


{-| cmpFunc: Cmp -> comparable -> comparable -> Bool -}
cmpFunc : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
cmpFunc cmpFuncArg cmpFuncArg0 cmpFuncArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "cmpFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cmp" []
                        , Type.var "comparable"
                        , Type.var "comparable"
                        ]
                        Type.bool
                    )
            }
        )
        [ cmpFuncArg, cmpFuncArg0, cmpFuncArg1 ]


annotation_ :
    { environment : Type.Annotation
    , funcType : Type.Annotation
    , cmp : Type.Annotation
    , func : Type.Annotation
    , op : Type.Annotation
    , program : Type.Annotation
    }
annotation_ =
    { environment = Type.namedWith [ "SECD", "Program" ] "Environment" []
    , funcType = Type.namedWith [ "SECD", "Program" ] "FuncType" []
    , cmp = Type.namedWith [ "SECD", "Program" ] "Cmp" []
    , func = Type.namedWith [ "SECD", "Program" ] "Func" []
    , op = Type.namedWith [ "SECD", "Program" ] "Op" []
    , program = Type.namedWith [ "SECD", "Program" ] "Program" []
    }


make_ :
    { env : Elm.Expression -> Elm.Expression
    , builtinFunc : Elm.Expression
    , lambdaFunc : Elm.Expression
    , loadedFunc : Elm.Expression
    , cMP_EQ : Elm.Expression
    , cMP_NE : Elm.Expression
    , cMP_LT : Elm.Expression
    , cMP_GT : Elm.Expression
    , cMP_LEQ : Elm.Expression
    , cMP_GEQ : Elm.Expression
    , aDD : Elm.Expression
    , mULT : Elm.Expression
    , sUB : Elm.Expression
    , aTOM : Elm.Expression
    , cONS : Elm.Expression
    , cAR : Elm.Expression
    , cDR : Elm.Expression
    , nULL : Elm.Expression
    , cOMPARE : Elm.Expression -> Elm.Expression
    , nIL : Elm.Expression
    , lD : Elm.Expression -> Elm.Expression
    , lDC : Elm.Expression -> Elm.Expression
    , lDF : Elm.Expression
    , aP : Elm.Expression
    , rTN : Elm.Expression
    , sEL : Elm.Expression
    , jOIN : Elm.Expression
    , rAP : Elm.Expression
    , dUM : Elm.Expression
    , fUNC : Elm.Expression -> Elm.Expression
    , nESTED : Elm.Expression -> Elm.Expression
    , fUNCBODY : Elm.Expression -> Elm.Expression -> Elm.Expression
    , program : Elm.Expression -> Elm.Expression
    }
make_ =
    { env =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "Env"
                    , annotation = Just (Type.namedWith [] "Environment" [])
                    }
                )
                [ ar0 ]
    , builtinFunc =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "BuiltinFunc"
            , annotation = Just (Type.namedWith [] "FuncType" [])
            }
    , lambdaFunc =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "LambdaFunc"
            , annotation = Just (Type.namedWith [] "FuncType" [])
            }
    , loadedFunc =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "LoadedFunc"
            , annotation = Just (Type.namedWith [] "FuncType" [])
            }
    , cMP_EQ =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CMP_EQ"
            , annotation = Just (Type.namedWith [] "Cmp" [])
            }
    , cMP_NE =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CMP_NE"
            , annotation = Just (Type.namedWith [] "Cmp" [])
            }
    , cMP_LT =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CMP_LT"
            , annotation = Just (Type.namedWith [] "Cmp" [])
            }
    , cMP_GT =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CMP_GT"
            , annotation = Just (Type.namedWith [] "Cmp" [])
            }
    , cMP_LEQ =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CMP_LEQ"
            , annotation = Just (Type.namedWith [] "Cmp" [])
            }
    , cMP_GEQ =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CMP_GEQ"
            , annotation = Just (Type.namedWith [] "Cmp" [])
            }
    , aDD =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "ADD"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , mULT =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "MULT"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , sUB =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "SUB"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , aTOM =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "ATOM"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , cONS =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CONS"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , cAR =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CAR"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , cDR =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "CDR"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , nULL =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "NULL"
            , annotation = Just (Type.namedWith [] "Func" [])
            }
    , cOMPARE =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "COMPARE"
                    , annotation = Just (Type.namedWith [] "Func" [])
                    }
                )
                [ ar0 ]
    , nIL =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "NIL"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , lD =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "LD"
                    , annotation = Just (Type.namedWith [] "Op" [])
                    }
                )
                [ ar0 ]
    , lDC =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "LDC"
                    , annotation = Just (Type.namedWith [] "Op" [])
                    }
                )
                [ ar0 ]
    , lDF =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "LDF"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , aP =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "AP"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , rTN =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "RTN"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , sEL =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "SEL"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , jOIN =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "JOIN"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , rAP =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "RAP"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , dUM =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "DUM"
            , annotation = Just (Type.namedWith [] "Op" [])
            }
    , fUNC =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "FUNC"
                    , annotation = Just (Type.namedWith [] "Op" [])
                    }
                )
                [ ar0 ]
    , nESTED =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "NESTED"
                    , annotation = Just (Type.namedWith [] "Op" [])
                    }
                )
                [ ar0 ]
    , fUNCBODY =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "FUNCBODY"
                    , annotation = Just (Type.namedWith [] "Op" [])
                    }
                )
                [ ar0, ar1 ]
    , program =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "Program"
                    , annotation = Just (Type.namedWith [] "Program" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { environment :
        Elm.Expression
        -> { environmentTags_0_0 | env : Elm.Expression -> Elm.Expression }
        -> Elm.Expression
    , funcType :
        Elm.Expression
        -> { funcTypeTags_1_0
            | builtinFunc : Elm.Expression
            , lambdaFunc : Elm.Expression
            , loadedFunc : Elm.Expression
        }
        -> Elm.Expression
    , cmp :
        Elm.Expression
        -> { cmpTags_2_0
            | cMP_EQ : Elm.Expression
            , cMP_NE : Elm.Expression
            , cMP_LT : Elm.Expression
            , cMP_GT : Elm.Expression
            , cMP_LEQ : Elm.Expression
            , cMP_GEQ : Elm.Expression
        }
        -> Elm.Expression
    , func :
        Elm.Expression
        -> { funcTags_3_0
            | aDD : Elm.Expression
            , mULT : Elm.Expression
            , sUB : Elm.Expression
            , aTOM : Elm.Expression
            , cONS : Elm.Expression
            , cAR : Elm.Expression
            , cDR : Elm.Expression
            , nULL : Elm.Expression
            , cOMPARE : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    , op :
        Elm.Expression
        -> { opTags_4_0
            | nIL : Elm.Expression
            , lD : Elm.Expression -> Elm.Expression
            , lDC : Elm.Expression -> Elm.Expression
            , lDF : Elm.Expression
            , aP : Elm.Expression
            , rTN : Elm.Expression
            , sEL : Elm.Expression
            , jOIN : Elm.Expression
            , rAP : Elm.Expression
            , dUM : Elm.Expression
            , fUNC : Elm.Expression -> Elm.Expression
            , nESTED : Elm.Expression -> Elm.Expression
            , fUNCBODY : Elm.Expression -> Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    , program :
        Elm.Expression
        -> { programTags_5_0 | program : Elm.Expression -> Elm.Expression }
        -> Elm.Expression
    }
caseOf_ =
    { environment =
        \environmentExpression environmentTags ->
            Elm.Case.custom
                environmentExpression
                (Type.namedWith [ "SECD", "Program" ] "Environment" [])
                [ Elm.Case.branch1
                    "Env"
                    ( "list.List", Type.list (Type.list Type.string) )
                    environmentTags.env
                ]
    , funcType =
        \funcTypeExpression funcTypeTags ->
            Elm.Case.custom
                funcTypeExpression
                (Type.namedWith [ "SECD", "Program" ] "FuncType" [])
                [ Elm.Case.branch0 "BuiltinFunc" funcTypeTags.builtinFunc
                , Elm.Case.branch0 "LambdaFunc" funcTypeTags.lambdaFunc
                , Elm.Case.branch0 "LoadedFunc" funcTypeTags.loadedFunc
                ]
    , cmp =
        \cmpExpression cmpTags ->
            Elm.Case.custom
                cmpExpression
                (Type.namedWith [ "SECD", "Program" ] "Cmp" [])
                [ Elm.Case.branch0 "CMP_EQ" cmpTags.cMP_EQ
                , Elm.Case.branch0 "CMP_NE" cmpTags.cMP_NE
                , Elm.Case.branch0 "CMP_LT" cmpTags.cMP_LT
                , Elm.Case.branch0 "CMP_GT" cmpTags.cMP_GT
                , Elm.Case.branch0 "CMP_LEQ" cmpTags.cMP_LEQ
                , Elm.Case.branch0 "CMP_GEQ" cmpTags.cMP_GEQ
                ]
    , func =
        \funcExpression funcTags ->
            Elm.Case.custom
                funcExpression
                (Type.namedWith [ "SECD", "Program" ] "Func" [])
                [ Elm.Case.branch0 "ADD" funcTags.aDD
                , Elm.Case.branch0 "MULT" funcTags.mULT
                , Elm.Case.branch0 "SUB" funcTags.sUB
                , Elm.Case.branch0 "ATOM" funcTags.aTOM
                , Elm.Case.branch0 "CONS" funcTags.cONS
                , Elm.Case.branch0 "CAR" funcTags.cAR
                , Elm.Case.branch0 "CDR" funcTags.cDR
                , Elm.Case.branch0 "NULL" funcTags.nULL
                , Elm.Case.branch1
                    "COMPARE"
                    ( "cmp", Type.namedWith [] "Cmp" [] )
                    funcTags.cOMPARE
                ]
    , op =
        \opExpression opTags ->
            Elm.Case.custom
                opExpression
                (Type.namedWith [ "SECD", "Program" ] "Op" [])
                [ Elm.Case.branch0 "NIL" opTags.nIL
                , Elm.Case.branch1
                    "LD"
                    ( "one", Type.tuple Type.int Type.int )
                    opTags.lD
                , Elm.Case.branch1 "LDC" ( "basics.Int", Type.int ) opTags.lDC
                , Elm.Case.branch0 "LDF" opTags.lDF
                , Elm.Case.branch0 "AP" opTags.aP
                , Elm.Case.branch0 "RTN" opTags.rTN
                , Elm.Case.branch0 "SEL" opTags.sEL
                , Elm.Case.branch0 "JOIN" opTags.jOIN
                , Elm.Case.branch0 "RAP" opTags.rAP
                , Elm.Case.branch0 "DUM" opTags.dUM
                , Elm.Case.branch1
                    "FUNC"
                    ( "func", Type.namedWith [] "Func" [] )
                    opTags.fUNC
                , Elm.Case.branch1
                    "NESTED"
                    ( "list.List", Type.list (Type.namedWith [] "Op" []) )
                    opTags.nESTED
                , Elm.Case.branch2
                    "FUNCBODY"
                    ( "string.String", Type.string )
                    ( "list.List", Type.list (Type.namedWith [] "Op" []) )
                    opTags.fUNCBODY
                ]
    , program =
        \programExpression programTags ->
            Elm.Case.custom
                programExpression
                (Type.namedWith [ "SECD", "Program" ] "Program" [])
                [ Elm.Case.branch1
                    "Program"
                    ( "list.List", Type.list (Type.namedWith [] "Op" []) )
                    programTags.program
                ]
    }


call_ :
    { encodeCompare : Elm.Expression -> Elm.Expression
    , encodeFunc : Elm.Expression -> Elm.Expression
    , encodeSingle : Elm.Expression -> Elm.Expression
    , encode : Elm.Expression -> Elm.Expression
    , addVarNames : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lookup : Elm.Expression -> Elm.Expression -> Elm.Expression
    , compileArgs :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , compileFunc : Elm.Expression -> Elm.Expression -> Elm.Expression
    , compileLetBinding : Elm.Expression -> Elm.Expression -> Elm.Expression
    , compileLetHelper :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , compileLetrec :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , compileLet :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , compileLambda :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , compileFuncApp :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , compileCons : Elm.Expression -> Elm.Expression
    , compile_ : Elm.Expression -> Elm.Expression -> Elm.Expression
    , compile : Elm.Expression -> Elm.Expression
    , fromSingleton : Elm.Expression -> Elm.Expression
    , fromList : Elm.Expression -> Elm.Expression
    , toList : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , cmpToString : Elm.Expression -> Elm.Expression
    , funcToString : Elm.Expression -> Elm.Expression
    , opToString : Elm.Expression -> Elm.Expression
    , cmpFunc :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { encodeCompare =
        \encodeCompareArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "encodeCompare"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Cmp" [] ]
                                (Type.namedWith [] "Value" [])
                            )
                    }
                )
                [ encodeCompareArg ]
    , encodeFunc =
        \encodeFuncArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "encodeFunc"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Func" [] ]
                                (Type.namedWith [] "Value" [])
                            )
                    }
                )
                [ encodeFuncArg ]
    , encodeSingle =
        \encodeSingleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "encodeSingle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Op" [] ]
                                (Type.namedWith [] "Value" [])
                            )
                    }
                )
                [ encodeSingleArg ]
    , encode =
        \encodeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "encode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Program" [] ]
                                (Type.namedWith [] "Value" [])
                            )
                    }
                )
                [ encodeArg ]
    , addVarNames =
        \addVarNamesArg addVarNamesArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "addVarNames"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string
                                , Type.namedWith [] "Environment" []
                                ]
                                (Type.namedWith [] "Environment" [])
                            )
                    }
                )
                [ addVarNamesArg, addVarNamesArg0 ]
    , lookup =
        \lookupArg lookupArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "lookup"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith [] "Environment" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.namedWith [] "Op" []
                                    ]
                                )
                            )
                    }
                )
                [ lookupArg, lookupArg0 ]
    , compileArgs =
        \compileArgsArg compileArgsArg0 compileArgsArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileArgs"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" []
                                , Type.bool
                                , Type.list (Type.namedWith [] "AST" [])
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compileArgsArg, compileArgsArg0, compileArgsArg1 ]
    , compileFunc =
        \compileFuncArg compileFuncArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileFunc"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" []
                                , Type.namedWith [] "AST" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.triple
                                        (Type.namedWith [] "Maybe" [ Type.int ])
                                        (Type.list (Type.namedWith [] "Op" []))
                                        Type.bool
                                    ]
                                )
                            )
                    }
                )
                [ compileFuncArg, compileFuncArg0 ]
    , compileLetBinding =
        \compileLetBindingArg compileLetBindingArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileLetBinding"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" []
                                , Type.tuple
                                    (Type.namedWith [] "Token" [])
                                    (Type.namedWith [] "AST" [])
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compileLetBindingArg, compileLetBindingArg0 ]
    , compileLetHelper =
        \compileLetHelperArg compileLetHelperArg0 compileLetHelperArg1 compileLetHelperArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileLetHelper"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool
                                , Type.namedWith [] "Environment" []
                                , Type.list
                                    (Type.tuple
                                        (Type.namedWith [] "Token" [])
                                        (Type.namedWith [] "AST" [])
                                    )
                                , Type.namedWith [] "AST" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compileLetHelperArg
                , compileLetHelperArg0
                , compileLetHelperArg1
                , compileLetHelperArg2
                ]
    , compileLetrec =
        \compileLetrecArg compileLetrecArg0 compileLetrecArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileLetrec"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" []
                                , Type.list
                                    (Type.tuple
                                        (Type.namedWith [] "Token" [])
                                        (Type.namedWith [] "AST" [])
                                    )
                                , Type.namedWith [] "AST" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compileLetrecArg, compileLetrecArg0, compileLetrecArg1 ]
    , compileLet =
        \compileLetArg compileLetArg0 compileLetArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileLet"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" []
                                , Type.list
                                    (Type.tuple
                                        (Type.namedWith [] "Token" [])
                                        (Type.namedWith [] "AST" [])
                                    )
                                , Type.namedWith [] "AST" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compileLetArg, compileLetArg0, compileLetArg1 ]
    , compileLambda =
        \compileLambdaArg compileLambdaArg0 compileLambdaArg1 compileLambdaArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileLambda"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.string ]
                                , Type.namedWith [] "Environment" []
                                , Type.list (Type.namedWith [] "Token" [])
                                , Type.namedWith [] "AST" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compileLambdaArg
                , compileLambdaArg0
                , compileLambdaArg1
                , compileLambdaArg2
                ]
    , compileFuncApp =
        \compileFuncAppArg compileFuncAppArg0 compileFuncAppArg1 compileFuncAppArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileFuncApp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" []
                                , Type.namedWith [] "AST" []
                                , Type.list (Type.namedWith [] "AST" [])
                                , Type.bool
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compileFuncAppArg
                , compileFuncAppArg0
                , compileFuncAppArg1
                , compileFuncAppArg2
                ]
    , compileCons =
        \compileConsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compileCons"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Cons" [ Type.int ] ]
                                (Type.list (Type.namedWith [] "Op" []))
                            )
                    }
                )
                [ compileConsArg ]
    , compile_ =
        \compile_Arg compile_Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compile_"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" []
                                , Type.namedWith [] "AST" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.list (Type.namedWith [] "Op" [])
                                    ]
                                )
                            )
                    }
                )
                [ compile_Arg, compile_Arg0 ]
    , compile =
        \compileArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "compile"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "AST" [] ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "Error" []
                                    , Type.namedWith [] "Program" []
                                    ]
                                )
                            )
                    }
                )
                [ compileArg ]
    , fromSingleton =
        \fromSingletonArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "fromSingleton"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Op" [] ]
                                (Type.namedWith [] "Program" [])
                            )
                    }
                )
                [ fromSingletonArg ]
    , fromList =
        \fromListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "fromList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.namedWith [] "Op" []) ]
                                (Type.namedWith [] "Program" [])
                            )
                    }
                )
                [ fromListArg ]
    , toList =
        \toListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "toList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Program" [] ]
                                (Type.list (Type.namedWith [] "Op" []))
                            )
                    }
                )
                [ toListArg ]
    , view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Op" [] ]
                                (Type.list
                                    (Type.namedWith
                                        []
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                )
                            )
                    }
                )
                [ viewArg ]
    , cmpToString =
        \cmpToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "cmpToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Cmp" [] ]
                                Type.string
                            )
                    }
                )
                [ cmpToStringArg ]
    , funcToString =
        \funcToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "funcToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Func" [] ]
                                Type.string
                            )
                    }
                )
                [ funcToStringArg ]
    , opToString =
        \opToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "opToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Op" [] ]
                                Type.string
                            )
                    }
                )
                [ opToStringArg ]
    , cmpFunc =
        \cmpFuncArg cmpFuncArg0 cmpFuncArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "Program" ]
                    , name = "cmpFunc"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Cmp" []
                                , Type.var "comparable"
                                , Type.var "comparable"
                                ]
                                Type.bool
                            )
                    }
                )
                [ cmpFuncArg, cmpFuncArg0, cmpFuncArg1 ]
    }


values_ :
    { singleDecoder : Elm.Expression
    , encodeCompare : Elm.Expression
    , encodeFunc : Elm.Expression
    , encodeSingle : Elm.Expression
    , decoder : Elm.Expression
    , encode : Elm.Expression
    , addVarNames : Elm.Expression
    , lookup : Elm.Expression
    , emptyEnv : Elm.Expression
    , compileArgs : Elm.Expression
    , compileFunc : Elm.Expression
    , compileLetBinding : Elm.Expression
    , compileLetHelper : Elm.Expression
    , compileLetrec : Elm.Expression
    , compileLet : Elm.Expression
    , compileLambda : Elm.Expression
    , compileFuncApp : Elm.Expression
    , compileCons : Elm.Expression
    , compile_ : Elm.Expression
    , compile : Elm.Expression
    , fromSingleton : Elm.Expression
    , fromList : Elm.Expression
    , toList : Elm.Expression
    , view : Elm.Expression
    , cmpToString : Elm.Expression
    , funcToString : Elm.Expression
    , opToString : Elm.Expression
    , cmpFunc : Elm.Expression
    }
values_ =
    { singleDecoder =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "singleDecoder"
            , annotation =
                Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "Op" [] ])
            }
    , encodeCompare =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encodeCompare"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cmp" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
    , encodeFunc =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encodeFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Func" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
    , encodeSingle =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encodeSingle"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Op" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
    , decoder =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "decoder"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Decoder"
                        [ Type.namedWith [] "Program" [] ]
                    )
            }
    , encode =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.namedWith [] "Value" [])
                    )
            }
    , addVarNames =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "addVarNames"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string
                        , Type.namedWith [] "Environment" []
                        ]
                        (Type.namedWith [] "Environment" [])
                    )
            }
    , lookup =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "lookup"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.namedWith [] "Environment" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.namedWith [] "Op" []
                            ]
                        )
                    )
            }
    , emptyEnv =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "emptyEnv"
            , annotation = Just (Type.namedWith [] "Environment" [])
            }
    , compileArgs =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileArgs"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.bool
                        , Type.list (Type.namedWith [] "AST" [])
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compileFunc =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.triple
                                (Type.namedWith [] "Maybe" [ Type.int ])
                                (Type.list (Type.namedWith [] "Op" []))
                                Type.bool
                            ]
                        )
                    )
            }
    , compileLetBinding =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLetBinding"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.tuple
                            (Type.namedWith [] "Token" [])
                            (Type.namedWith [] "AST" [])
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compileLetHelper =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLetHelper"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool
                        , Type.namedWith [] "Environment" []
                        , Type.list
                            (Type.tuple
                                (Type.namedWith [] "Token" [])
                                (Type.namedWith [] "AST" [])
                            )
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compileLetrec =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLetrec"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.list
                            (Type.tuple
                                (Type.namedWith [] "Token" [])
                                (Type.namedWith [] "AST" [])
                            )
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compileLet =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLet"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.list
                            (Type.tuple
                                (Type.namedWith [] "Token" [])
                                (Type.namedWith [] "AST" [])
                            )
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compileLambda =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileLambda"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.string ]
                        , Type.namedWith [] "Environment" []
                        , Type.list (Type.namedWith [] "Token" [])
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compileFuncApp =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileFuncApp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.namedWith [] "AST" []
                        , Type.list (Type.namedWith [] "AST" [])
                        , Type.bool
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compileCons =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compileCons"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cons" [ Type.int ] ]
                        (Type.list (Type.namedWith [] "Op" []))
                    )
            }
    , compile_ =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compile_"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" []
                        , Type.namedWith [] "AST" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.list (Type.namedWith [] "Op" [])
                            ]
                        )
                    )
            }
    , compile =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "compile"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "AST" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.namedWith [] "Error" []
                            , Type.namedWith [] "Program" []
                            ]
                        )
                    )
            }
    , fromSingleton =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "fromSingleton"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Op" [] ]
                        (Type.namedWith [] "Program" [])
                    )
            }
    , fromList =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "fromList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [] "Op" []) ]
                        (Type.namedWith [] "Program" [])
                    )
            }
    , toList =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "toList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.list (Type.namedWith [] "Op" []))
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Op" [] ]
                        (Type.list
                            (Type.namedWith [] "Element" [ Type.var "msg" ])
                        )
                    )
            }
    , cmpToString =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "cmpToString"
            , annotation =
                Just (Type.function [ Type.namedWith [] "Cmp" [] ] Type.string)
            }
    , funcToString =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "funcToString"
            , annotation =
                Just (Type.function [ Type.namedWith [] "Func" [] ] Type.string)
            }
    , opToString =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "opToString"
            , annotation =
                Just (Type.function [ Type.namedWith [] "Op" [] ] Type.string)
            }
    , cmpFunc =
        Elm.value
            { importFrom = [ "SECD", "Program" ]
            , name = "cmpFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cmp" []
                        , Type.var "comparable"
                        , Type.var "comparable"
                        ]
                        Type.bool
                    )
            }
    }


