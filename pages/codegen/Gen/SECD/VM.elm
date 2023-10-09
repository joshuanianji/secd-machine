module Gen.SECD.VM exposing (addHoverable, annotation_, applyFunction, boolToValue, call_, caseOf_, contextDecoder, controlDecoder, decoder, dumpDecoder, encode, encodeContext, encodeControl, encodeDump, encodeEnvironment, encodeStack, encodeValue, endIfElse, endProgram, environmentDecoder, evalBinary, evalChunk, evalFunc, evalIfElse, evalPage, evalUnary, evaluate, getPages, init, initCtx, initRaw, initState, loadFromEnv, loadFunction, locateInEnv, make_, moduleName_, nil, recursiveApply, returnFromFunction, stackDecoder, stateStep, step, stepN, stepNAccumulate, valueDecoder, valueToString, values_, view, viewContext, viewControl, viewDump, viewDumpVal, viewEnv, viewStack, viewStackName, viewVMStack, viewValue, vmAdd, vmAtom, vmCar, vmCdr, vmCompare, vmCons, vmMultiply, vmNull, vmSub)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, initCtx, boolToValue, nil, vmAdd, vmMultiply, vmSub, vmAtom, vmCons, vmCar, vmCdr, vmCompare, vmNull, valueToString, init, initRaw, initState, stateStep, stepN, step, endProgram, loadFromEnv, locateInEnv, evalFunc, evalBinary, evalUnary, evalIfElse, endIfElse, loadFunction, applyFunction, returnFromFunction, recursiveApply, evaluate, stepNAccumulate, evalChunk, evalPage, getPages, view, viewContext, viewVMStack, viewValue, viewStack, viewEnv, viewControl, viewDump, viewDumpVal, viewStackName, addHoverable, encode, decoder, encodeContext, contextDecoder, encodeStack, stackDecoder, encodeEnvironment, environmentDecoder, encodeControl, controlDecoder, encodeDump, dumpDecoder, encodeValue, valueDecoder, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "SECD", "VM" ]


{-| valueDecoder: Decoder Value -}
valueDecoder : Elm.Expression
valueDecoder =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "valueDecoder"
        , annotation =
            Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "Value" [] ])
        }


{-| encodeValue: Value -> Encode.Value -}
encodeValue : Elm.Expression -> Elm.Expression
encodeValue encodeValueArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeValue"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
        )
        [ encodeValueArg ]


{-| dumpDecoder: Decoder Dump -}
dumpDecoder : Elm.Expression
dumpDecoder =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "dumpDecoder"
        , annotation =
            Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "Dump" [] ])
        }


{-| encodeDump: Dump -> Encode.Value -}
encodeDump : Elm.Expression -> Elm.Expression
encodeDump encodeDumpArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeDump"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Dump" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
        )
        [ encodeDumpArg ]


{-| controlDecoder: Decoder Control -}
controlDecoder : Elm.Expression
controlDecoder =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "controlDecoder"
        , annotation =
            Just
                (Type.namedWith [] "Decoder" [ Type.namedWith [] "Control" [] ])
        }


{-| encodeControl: Control -> Encode.Value -}
encodeControl : Elm.Expression -> Elm.Expression
encodeControl encodeControlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeControl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Control" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
        )
        [ encodeControlArg ]


{-| environmentDecoder: Decoder Environment -}
environmentDecoder : Elm.Expression
environmentDecoder =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "environmentDecoder"
        , annotation =
            Just
                (Type.namedWith
                    []
                    "Decoder"
                    [ Type.namedWith [] "Environment" [] ]
                )
        }


{-| encodeEnvironment: Environment -> Encode.Value -}
encodeEnvironment : Elm.Expression -> Elm.Expression
encodeEnvironment encodeEnvironmentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeEnvironment"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
        )
        [ encodeEnvironmentArg ]


{-| stackDecoder: Decoder Stack -}
stackDecoder : Elm.Expression
stackDecoder =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "stackDecoder"
        , annotation =
            Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "Stack" [] ])
        }


{-| encodeStack: Stack -> Encode.Value -}
encodeStack : Elm.Expression -> Elm.Expression
encodeStack encodeStackArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeStack"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Stack" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
        )
        [ encodeStackArg ]


{-| contextDecoder: Decoder Context -}
contextDecoder : Elm.Expression
contextDecoder =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "contextDecoder"
        , annotation =
            Just
                (Type.namedWith [] "Decoder" [ Type.namedWith [] "Context" [] ])
        }


{-| encodeContext: Context -> Encode.Value -}
encodeContext : Elm.Expression -> Elm.Expression
encodeContext encodeContextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeContext"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Context" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
        )
        [ encodeContextArg ]


{-| decoder: Decoder VM -}
decoder : Elm.Expression
decoder =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "decoder"
        , annotation =
            Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "VM" [] ])
        }


{-| encode: VM -> Encode.Value -}
encode : Elm.Expression -> Elm.Expression
encode encodeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
        )
        [ encodeArg ]


{-| addHoverable: List (Attribute msg) -> List (Attribute msg) -}
addHoverable : List Elm.Expression -> Elm.Expression
addHoverable addHoverableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "addHoverable"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        ]
                        (Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        )
                    )
            }
        )
        [ Elm.list addHoverableArg ]


{-| viewStackName: String -> List a -> Element msg -}
viewStackName : String -> List Elm.Expression -> Elm.Expression
viewStackName viewStackNameArg viewStackNameArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewStackName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.list (Type.var "a") ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.string viewStackNameArg, Elm.list viewStackNameArg0 ]


{-| viewDumpVal: Maybe Int -> DumpValue -> Element msg -}
viewDumpVal : Elm.Expression -> Elm.Expression -> Elm.Expression
viewDumpVal viewDumpValArg viewDumpValArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewDumpVal"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "DumpValue" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewDumpValArg, viewDumpValArg0 ]


{-| viewDump: Maybe Int -> Dump -> Element msg -}
viewDump : Elm.Expression -> Elm.Expression -> Elm.Expression
viewDump viewDumpArg viewDumpArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewDump"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Dump" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewDumpArg, viewDumpArg0 ]


{-| viewControl: Maybe Int -> Control -> Element msg -}
viewControl : Elm.Expression -> Elm.Expression -> Elm.Expression
viewControl viewControlArg viewControlArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewControl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Control" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewControlArg, viewControlArg0 ]


{-| viewEnv: Maybe Int -> Environment -> Element msg -}
viewEnv : Elm.Expression -> Elm.Expression -> Elm.Expression
viewEnv viewEnvArg viewEnvArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewEnv"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Environment" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewEnvArg, viewEnvArg0 ]


{-| viewStack: Maybe Int -> Stack -> Element msg -}
viewStack : Elm.Expression -> Elm.Expression -> Elm.Expression
viewStack viewStackArg viewStackArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewStack"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Stack" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewStackArg, viewStackArg0 ]


{-| viewValue: Maybe Int -> Value -> Element msg -}
viewValue : Elm.Expression -> Elm.Expression -> Elm.Expression
viewValue viewValueArg viewValueArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewValue"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewValueArg, viewValueArg0 ]


{-| viewVMStack: 
    { n : Maybe Int
    , viewStackChunk : List a -> List (Element msg)
    , stack : List a
    , stackName : String
    }
    -> Element msg
-}
viewVMStack :
    { n : Elm.Expression
    , viewStackChunk : Elm.Expression -> Elm.Expression
    , stack : List Elm.Expression
    , stackName : String
    }
    -> Elm.Expression
viewVMStack viewVMStackArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewVMStack"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "n", Type.namedWith [] "Maybe" [ Type.int ] )
                            , ( "viewStackChunk"
                              , Type.function
                                    [ Type.list (Type.var "a") ]
                                    (Type.list
                                        (Type.namedWith
                                            []
                                            "Element"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "stack", Type.list (Type.var "a") )
                            , ( "stackName", Type.string )
                            ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "n" viewVMStackArg.n
            , Tuple.pair
                "viewStackChunk"
                (Elm.functionReduced
                    "viewVMStackUnpack"
                    viewVMStackArg.viewStackChunk
                )
            , Tuple.pair "stack" (Elm.list viewVMStackArg.stack)
            , Tuple.pair "stackName" (Elm.string viewVMStackArg.stackName)
            ]
        ]


{-| viewContext: Maybe Int -> Context -> Element msg -}
viewContext : Elm.Expression -> Elm.Expression -> Elm.Expression
viewContext viewContextArg viewContextArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewContext"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Context" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewContextArg, viewContextArg0 ]


{-| view: ViewOptions -> VM -> Element msg -}
view : Elm.Expression -> Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "ViewOptions" []
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
        )
        [ viewArg, viewArg0 ]


{-| getPages: { maxPages : Int, pageSize : Int, chunkSize : Int } -> VM -> PagesData -}
getPages :
    { maxPages : Int, pageSize : Int, chunkSize : Int }
    -> Elm.Expression
    -> Elm.Expression
getPages getPagesArg getPagesArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "getPages"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "maxPages", Type.int )
                            , ( "pageSize", Type.int )
                            , ( "chunkSize", Type.int )
                            ]
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "PagesData" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "maxPages" (Elm.int getPagesArg.maxPages)
            , Tuple.pair "pageSize" (Elm.int getPagesArg.pageSize)
            , Tuple.pair "chunkSize" (Elm.int getPagesArg.chunkSize)
            ]
        , getPagesArg0
        ]


{-| {-|


## evalPage

Given a starting VM state, evaluate a page starting from that state.

Each page can hold `pageSize` chunks, and each chunk holds `chunkSize` states
Thus, each page is limited to `pageSize * chunkSize` states

We accumulate the first VM state of each chunk

  - if we're not finished, return `(totalVMs, chunkVMs, Err newVMState)`
  - If we're finished, return `(totalVMs, chunkVMs, Ok vmResult)`

If the evaluation does not terminate, `newVMState` is fresh VM state to start the next calculation off of

**Note** chunkVMs does not include the input VM. Thus, totalVMCount is one more than `List.length chunkVMs`.
This helps us create the Zipper more easily, although the code is a little bit messier...

-}

evalPage: 
    Int
    -> Int
    -> VM
    -> { totalVMCount : Int, chunkVMs : List VM, result : Result VM VMResult }
-}
evalPage : Int -> Int -> Elm.Expression -> Elm.Expression
evalPage evalPageArg evalPageArg0 evalPageArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalPage"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int, Type.namedWith [] "VM" [] ]
                        (Type.record
                            [ ( "totalVMCount", Type.int )
                            , ( "chunkVMs"
                              , Type.list (Type.namedWith [] "VM" [])
                              )
                            , ( "result"
                              , Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "VM" []
                                    , Type.namedWith [] "VMResult" []
                                    ]
                              )
                            ]
                        )
                    )
            }
        )
        [ Elm.int evalPageArg, Elm.int evalPageArg0, evalPageArg1 ]


{-| {-|


## evalChunk

Given a starting VM, evaluate the chunk starting from that VM.

Note that, compared to evalN, evalChunk only keeps the first VM state.

  - If we're not finished, return `(count, Err nextVM)`
  - If we're finished, return `(count, Ok vmResult)`

`nextVM` is the vm you would want to start off of for the next chunk. It is not included in the current chunk.

`count` is the total size of the chunk. The count includes the inital VM state


### Dev notes:

This function is essentially `stepNAccumulate n (chunkSize - 1)`. The reason I subtract 1 from the chunk size is that
the initial VM state is also included in the chunk, so to get a total of `n` states in the chunk, I step (n-1) times.

-}

evalChunk: VM -> Int -> ( Int, Result VM VMResult )
-}
evalChunk : Elm.Expression -> Int -> Elm.Expression
evalChunk evalChunkArg evalChunkArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalChunk"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [], Type.int ]
                        (Type.tuple
                            Type.int
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.namedWith [] "VM" []
                                , Type.namedWith [] "VMResult" []
                                ]
                            )
                        )
                    )
            }
        )
        [ evalChunkArg, Elm.int evalChunkArg0 ]


{-| {-|


## stepNAccumulate

Given a starting VM, step a max of "n" times, storing each VM state in a list.

`evalN n startVM` does not include startVM in the states we return.

  - If we're not finished, return `(vms, Err nextVM)`
  - If we're finished, return `(vms, Ok (vmResult, count))`

`nextVM` is not included in the accumulated states.

The VM returned if the program is not finished executing is also not included in the list of VM states

-}

stepNAccumulate: Int -> VM -> ( List VM, Result VM VMResult )
-}
stepNAccumulate : Int -> Elm.Expression -> Elm.Expression
stepNAccumulate stepNAccumulateArg stepNAccumulateArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "stepNAccumulate"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [] "VM" [] ]
                        (Type.tuple
                            (Type.list (Type.namedWith [] "VM" []))
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.namedWith [] "VM" []
                                , Type.namedWith [] "VMResult" []
                                ]
                            )
                        )
                    )
            }
        )
        [ Elm.int stepNAccumulateArg, stepNAccumulateArg0 ]


{-| {-|


## evaluate

fully evaluates a VM, counting the amount of steps

_this is mainly used for testing_

**WARNING**: this will blow up the stack if it goes in an infinite loop

-}

evaluate: VM -> ( Int, VMResult )
-}
evaluate : Elm.Expression -> Elm.Expression
evaluate evaluateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evaluate"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.tuple Type.int (Type.namedWith [] "VMResult" []))
                    )
            }
        )
        [ evaluateArg ]


{-| recursiveApply: VM -> State -}
recursiveApply : Elm.Expression -> Elm.Expression
recursiveApply recursiveApplyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "recursiveApply"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ recursiveApplyArg ]


{-| returnFromFunction: VM -> State -}
returnFromFunction : Elm.Expression -> Elm.Expression
returnFromFunction returnFromFunctionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "returnFromFunction"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ returnFromFunctionArg ]


{-| applyFunction: VM -> State -}
applyFunction : Elm.Expression -> Elm.Expression
applyFunction applyFunctionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "applyFunction"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ applyFunctionArg ]


{-| loadFunction: VM -> State -}
loadFunction : Elm.Expression -> Elm.Expression
loadFunction loadFunctionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "loadFunction"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ loadFunctionArg ]


{-| endIfElse: VM -> State -}
endIfElse : Elm.Expression -> Elm.Expression
endIfElse endIfElseArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "endIfElse"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ endIfElseArg ]


{-| evalIfElse: VM -> State -}
evalIfElse : Elm.Expression -> Elm.Expression
evalIfElse evalIfElseArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalIfElse"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ evalIfElseArg ]


{-| evalUnary: (Value -> Result String Value) -> VM -> State -}
evalUnary :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
evalUnary evalUnaryArg evalUnaryArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalUnary"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith [] "Value" [] ]
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.string, Type.namedWith [] "Value" [] ]
                            )
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ Elm.functionReduced "evalUnaryUnpack" evalUnaryArg, evalUnaryArg0 ]


{-| evalBinary: (Value -> Value -> Result String Value) -> VM -> State -}
evalBinary :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
evalBinary evalBinaryArg evalBinaryArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalBinary"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith [] "Value" []
                            , Type.namedWith [] "Value" []
                            ]
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.string, Type.namedWith [] "Value" [] ]
                            )
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ Elm.functionReduced
            "evalBinaryUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (evalBinaryArg functionReducedUnpack)
            )
        , evalBinaryArg0
        ]


{-| evalFunc: Func -> VM -> State -}
evalFunc : Elm.Expression -> Elm.Expression -> Elm.Expression
evalFunc evalFuncArg evalFuncArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Func" []
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ evalFuncArg, evalFuncArg0 ]


{-| locateInEnv: ( Int, Int ) -> Environment -> Context -> Result String (Cons Value) -}
locateInEnv :
    Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
locateInEnv locateInEnvArg locateInEnvArg0 locateInEnvArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "locateInEnv"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple Type.int Type.int
                        , Type.namedWith [] "Environment" []
                        , Type.namedWith [] "Context" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string
                            , Type.namedWith
                                []
                                "Cons"
                                [ Type.namedWith [] "Value" [] ]
                            ]
                        )
                    )
            }
        )
        [ locateInEnvArg, locateInEnvArg0, locateInEnvArg1 ]


{-| loadFromEnv: ( Int, Int ) -> VM -> State -}
loadFromEnv : Elm.Expression -> Elm.Expression -> Elm.Expression
loadFromEnv loadFromEnvArg loadFromEnvArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "loadFromEnv"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple Type.int Type.int
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ loadFromEnvArg, loadFromEnvArg0 ]


{-| endProgram: Stack -> VM -> State -}
endProgram : Elm.Expression -> Elm.Expression -> Elm.Expression
endProgram endProgramArg endProgramArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "endProgram"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Stack" []
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ endProgramArg, endProgramArg0 ]


{-| step: VM -> State -}
step : Elm.Expression -> Elm.Expression
step stepArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "step"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ stepArg ]


{-| stepN: Int -> VM -> State -}
stepN : Int -> Elm.Expression -> Elm.Expression
stepN stepNArg stepNArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "stepN"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ Elm.int stepNArg, stepNArg0 ]


{-| stateStep: State -> State -}
stateStep : Elm.Expression -> Elm.Expression
stateStep stateStepArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "stateStep"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "State" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ stateStepArg ]


{-| initState: VM -> State -}
initState : Elm.Expression -> Elm.Expression
initState initStateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "initState"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ initStateArg ]


{-| initRaw: Program -> VM -}
initRaw : Elm.Expression -> Elm.Expression
initRaw initRawArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "initRaw"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.namedWith [] "VM" [])
                    )
            }
        )
        [ initRawArg ]


{-| init: Program -> State -}
init : Elm.Expression -> Elm.Expression
init initArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
        )
        [ initArg ]


{-| valueToString: Value -> String -}
valueToString : Elm.Expression -> Elm.Expression
valueToString valueToStringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "valueToString"
            , annotation =
                Just
                    (Type.function [ Type.namedWith [] "Value" [] ] Type.string)
            }
        )
        [ valueToStringArg ]


{-| vmNull: Value -> Result String Value -}
vmNull : Elm.Expression -> Elm.Expression
vmNull vmNullArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmNull"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmNullArg ]


{-| vmCompare: Cmp -> Value -> Value -> Result String Value -}
vmCompare : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
vmCompare vmCompareArg vmCompareArg0 vmCompareArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCompare"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cmp" []
                        , Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmCompareArg, vmCompareArg0, vmCompareArg1 ]


{-| vmCdr: Value -> Result String Value -}
vmCdr : Elm.Expression -> Elm.Expression
vmCdr vmCdrArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCdr"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmCdrArg ]


{-| vmCar: Value -> Result String Value -}
vmCar : Elm.Expression -> Elm.Expression
vmCar vmCarArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCar"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmCarArg ]


{-| vmCons: Value -> Value -> Result String Value -}
vmCons : Elm.Expression -> Elm.Expression -> Elm.Expression
vmCons vmConsArg vmConsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCons"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmConsArg, vmConsArg0 ]


{-| vmAtom: Value -> Result String Value -}
vmAtom : Elm.Expression -> Elm.Expression
vmAtom vmAtomArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmAtom"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmAtomArg ]


{-| vmSub: Value -> Value -> Result String Value -}
vmSub : Elm.Expression -> Elm.Expression -> Elm.Expression
vmSub vmSubArg vmSubArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmSub"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmSubArg, vmSubArg0 ]


{-| vmMultiply: Value -> Value -> Result String Value -}
vmMultiply : Elm.Expression -> Elm.Expression -> Elm.Expression
vmMultiply vmMultiplyArg vmMultiplyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmMultiply"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmMultiplyArg, vmMultiplyArg0 ]


{-| vmAdd: Value -> Value -> Result String Value -}
vmAdd : Elm.Expression -> Elm.Expression -> Elm.Expression
vmAdd vmAddArg vmAddArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmAdd"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
        )
        [ vmAddArg, vmAddArg0 ]


{-| nil: Value -}
nil : Elm.Expression
nil =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "nil"
        , annotation = Just (Type.namedWith [] "Value" [])
        }


{-| boolToValue: Bool -> Value -}
boolToValue : Bool -> Elm.Expression
boolToValue boolToValueArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "boolToValue"
            , annotation =
                Just
                    (Type.function [ Type.bool ] (Type.namedWith [] "Value" []))
            }
        )
        [ Elm.bool boolToValueArg ]


{-| initCtx: Context -}
initCtx : Elm.Expression
initCtx =
    Elm.value
        { importFrom = [ "SECD", "VM" ]
        , name = "initCtx"
        , annotation = Just (Type.namedWith [] "Context" [])
        }


annotation_ :
    { viewOptions : Type.Annotation
    , pagesData : Type.Annotation
    , vMResult : Type.Annotation
    , dump : Type.Annotation
    , control : Type.Annotation
    , environment : Type.Annotation
    , stack : Type.Annotation
    , context : Type.Annotation
    , align : Type.Annotation
    , state : Type.Annotation
    , dumpValue : Type.Annotation
    , value : Type.Annotation
    , vM : Type.Annotation
    }
annotation_ =
    { viewOptions =
        Type.alias
            moduleName_
            "ViewOptions"
            []
            (Type.record
                [ ( "depth", Type.namedWith [] "Maybe" [ Type.int ] )
                , ( "rowView", Type.bool )
                , ( "aligns", Type.namedWith [] "Align" [] )
                ]
            )
    , pagesData =
        Type.alias
            moduleName_
            "PagesData"
            []
            (Type.record
                [ ( "result"
                  , Type.namedWith
                        []
                        "Result"
                        [ Type.namedWith [] "VM" []
                        , Type.namedWith [] "VMResult" []
                        ]
                  )
                , ( "initialPage"
                  , Type.namedWith [] "Zipper" [ Type.namedWith [] "VM" [] ]
                  )
                , ( "initialChunk"
                  , Type.namedWith [] "Zipper" [ Type.namedWith [] "VM" [] ]
                  )
                , ( "pages", Type.namedWith [] "Zipper" [ Type.int ] )
                , ( "totalVMCount", Type.int )
                , ( "toJSValues"
                  , Type.list
                        (Type.tuple
                            Type.int
                            (Type.namedWith [ "Encode" ] "Value" [])
                        )
                  )
                ]
            )
    , vMResult =
        Type.alias
            moduleName_
            "VMResult"
            []
            (Type.namedWith
                []
                "Result"
                [ Type.namedWith [] "Error" [], Type.namedWith [] "Value" [] ]
            )
    , dump =
        Type.alias
            moduleName_
            "Dump"
            []
            (Type.list (Type.namedWith [] "DumpValue" []))
    , control =
        Type.alias
            moduleName_
            "Control"
            []
            (Type.list (Type.namedWith [] "Op" []))
    , environment =
        Type.alias
            moduleName_
            "Environment"
            []
            (Type.list
                (Type.namedWith [] "EnvItem" [ Type.namedWith [] "Value" [] ])
            )
    , stack =
        Type.alias
            moduleName_
            "Stack"
            []
            (Type.list (Type.namedWith [] "Value" []))
    , context =
        Type.alias
            moduleName_
            "Context"
            []
            (Type.record
                [ ( "dummyVal"
                  , Type.namedWith
                        []
                        "Maybe"
                        [ Type.list
                            (Type.namedWith
                                []
                                "Cons"
                                [ Type.namedWith [] "Value" [] ]
                            )
                        ]
                  )
                ]
            )
    , align = Type.namedWith [ "SECD", "VM" ] "Align" []
    , state = Type.namedWith [ "SECD", "VM" ] "State" []
    , dumpValue = Type.namedWith [ "SECD", "VM" ] "DumpValue" []
    , value = Type.namedWith [ "SECD", "VM" ] "Value" []
    , vM = Type.namedWith [ "SECD", "VM" ] "VM" []
    }


make_ :
    { viewOptions :
        { depth : Elm.Expression
        , rowView : Elm.Expression
        , aligns : Elm.Expression
        }
        -> Elm.Expression
    , pagesData :
        { result : Elm.Expression
        , initialPage : Elm.Expression
        , initialChunk : Elm.Expression
        , pages : Elm.Expression
        , totalVMCount : Elm.Expression
        , toJSValues : Elm.Expression
        }
        -> Elm.Expression
    , context : { dummyVal : Elm.Expression } -> Elm.Expression
    , center : Elm.Expression
    , left : Elm.Expression
    , right : Elm.Expression
    , unfinished : Elm.Expression -> Elm.Expression
    , finished : Elm.Expression -> Elm.Expression -> Elm.Expression
    , error : Elm.Expression -> Elm.Expression -> Elm.Expression
    , control : Elm.Expression -> Elm.Expression
    , entireState :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , integer : Elm.Expression -> Elm.Expression
    , truthy : Elm.Expression
    , array : Elm.Expression -> Elm.Expression
    , closure :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , vM :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    }
make_ =
    { viewOptions =
        \viewOptions_args ->
            Elm.withType
                (Type.alias
                    [ "SECD", "VM" ]
                    "ViewOptions"
                    []
                    (Type.record
                        [ ( "depth", Type.namedWith [] "Maybe" [ Type.int ] )
                        , ( "rowView", Type.bool )
                        , ( "aligns", Type.namedWith [] "Align" [] )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "depth" viewOptions_args.depth
                    , Tuple.pair "rowView" viewOptions_args.rowView
                    , Tuple.pair "aligns" viewOptions_args.aligns
                    ]
                )
    , pagesData =
        \pagesData_args ->
            Elm.withType
                (Type.alias
                    [ "SECD", "VM" ]
                    "PagesData"
                    []
                    (Type.record
                        [ ( "result"
                          , Type.namedWith
                                []
                                "Result"
                                [ Type.namedWith [] "VM" []
                                , Type.namedWith [] "VMResult" []
                                ]
                          )
                        , ( "initialPage"
                          , Type.namedWith
                                []
                                "Zipper"
                                [ Type.namedWith [] "VM" [] ]
                          )
                        , ( "initialChunk"
                          , Type.namedWith
                                []
                                "Zipper"
                                [ Type.namedWith [] "VM" [] ]
                          )
                        , ( "pages", Type.namedWith [] "Zipper" [ Type.int ] )
                        , ( "totalVMCount", Type.int )
                        , ( "toJSValues"
                          , Type.list
                                (Type.tuple
                                    Type.int
                                    (Type.namedWith [ "Encode" ] "Value" [])
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "result" pagesData_args.result
                    , Tuple.pair "initialPage" pagesData_args.initialPage
                    , Tuple.pair "initialChunk" pagesData_args.initialChunk
                    , Tuple.pair "pages" pagesData_args.pages
                    , Tuple.pair "totalVMCount" pagesData_args.totalVMCount
                    , Tuple.pair "toJSValues" pagesData_args.toJSValues
                    ]
                )
    , context =
        \context_args ->
            Elm.withType
                (Type.alias
                    [ "SECD", "VM" ]
                    "Context"
                    []
                    (Type.record
                        [ ( "dummyVal"
                          , Type.namedWith
                                []
                                "Maybe"
                                [ Type.list
                                    (Type.namedWith
                                        []
                                        "Cons"
                                        [ Type.namedWith [] "Value" [] ]
                                    )
                                ]
                          )
                        ]
                    )
                )
                (Elm.record [ Tuple.pair "dummyVal" context_args.dummyVal ])
    , center =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "Center"
            , annotation = Just (Type.namedWith [] "Align" [])
            }
    , left =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "Left"
            , annotation = Just (Type.namedWith [] "Align" [])
            }
    , right =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "Right"
            , annotation = Just (Type.namedWith [] "Align" [])
            }
    , unfinished =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "Unfinished"
                    , annotation = Just (Type.namedWith [] "State" [])
                    }
                )
                [ ar0 ]
    , finished =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "Finished"
                    , annotation = Just (Type.namedWith [] "State" [])
                    }
                )
                [ ar0, ar1 ]
    , error =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "Error"
                    , annotation = Just (Type.namedWith [] "State" [])
                    }
                )
                [ ar0, ar1 ]
    , control =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "Control"
                    , annotation = Just (Type.namedWith [] "DumpValue" [])
                    }
                )
                [ ar0 ]
    , entireState =
        \ar0 ar1 ar2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "EntireState"
                    , annotation = Just (Type.namedWith [] "DumpValue" [])
                    }
                )
                [ ar0, ar1, ar2 ]
    , integer =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "Integer"
                    , annotation = Just (Type.namedWith [] "Value" [])
                    }
                )
                [ ar0 ]
    , truthy =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "Truthy"
            , annotation = Just (Type.namedWith [] "Value" [])
            }
    , array =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "Array"
                    , annotation = Just (Type.namedWith [] "Value" [])
                    }
                )
                [ ar0 ]
    , closure =
        \ar0 ar1 ar2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "Closure"
                    , annotation = Just (Type.namedWith [] "Value" [])
                    }
                )
                [ ar0, ar1, ar2 ]
    , vM =
        \ar0 ar1 ar2 ar3 ar4 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "VM"
                    , annotation = Just (Type.namedWith [] "VM" [])
                    }
                )
                [ ar0, ar1, ar2, ar3, ar4 ]
    }


caseOf_ :
    { align :
        Elm.Expression
        -> { alignTags_0_0
            | center : Elm.Expression
            , left : Elm.Expression
            , right : Elm.Expression
        }
        -> Elm.Expression
    , state :
        Elm.Expression
        -> { stateTags_1_0
            | unfinished : Elm.Expression -> Elm.Expression
            , finished : Elm.Expression -> Elm.Expression -> Elm.Expression
            , error : Elm.Expression -> Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    , dumpValue :
        Elm.Expression
        -> { dumpValueTags_2_0
            | control : Elm.Expression -> Elm.Expression
            , entireState :
                Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
        }
        -> Elm.Expression
    , value :
        Elm.Expression
        -> { valueTags_3_0
            | integer : Elm.Expression -> Elm.Expression
            , truthy : Elm.Expression
            , array : Elm.Expression -> Elm.Expression
            , closure :
                Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
        }
        -> Elm.Expression
    , vM :
        Elm.Expression
        -> { vMTags_4_0
            | vM :
                Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { align =
        \alignExpression alignTags ->
            Elm.Case.custom
                alignExpression
                (Type.namedWith [ "SECD", "VM" ] "Align" [])
                [ Elm.Case.branch0 "Center" alignTags.center
                , Elm.Case.branch0 "Left" alignTags.left
                , Elm.Case.branch0 "Right" alignTags.right
                ]
    , state =
        \stateExpression stateTags ->
            Elm.Case.custom
                stateExpression
                (Type.namedWith [ "SECD", "VM" ] "State" [])
                [ Elm.Case.branch1
                    "Unfinished"
                    ( "vM", Type.namedWith [] "VM" [] )
                    stateTags.unfinished
                , Elm.Case.branch2
                    "Finished"
                    ( "vM", Type.namedWith [] "VM" [] )
                    ( "value", Type.namedWith [] "Value" [] )
                    stateTags.finished
                , Elm.Case.branch2
                    "Error"
                    ( "vM", Type.namedWith [] "VM" [] )
                    ( "string.String", Type.string )
                    stateTags.error
                ]
    , dumpValue =
        \dumpValueExpression dumpValueTags ->
            Elm.Case.custom
                dumpValueExpression
                (Type.namedWith [ "SECD", "VM" ] "DumpValue" [])
                [ Elm.Case.branch1
                    "Control"
                    ( "list.List", Type.list (Type.namedWith [] "Op" []) )
                    dumpValueTags.control
                , Elm.Case.branch3
                    "EntireState"
                    ( "stack", Type.namedWith [] "Stack" [] )
                    ( "environment", Type.namedWith [] "Environment" [] )
                    ( "control", Type.namedWith [] "Control" [] )
                    dumpValueTags.entireState
                ]
    , value =
        \valueExpression valueTags ->
            Elm.Case.custom
                valueExpression
                (Type.namedWith [ "SECD", "VM" ] "Value" [])
                [ Elm.Case.branch1
                    "Integer"
                    ( "basics.Int", Type.int )
                    valueTags.integer
                , Elm.Case.branch0 "Truthy" valueTags.truthy
                , Elm.Case.branch1
                    "Array"
                    ( "cons"
                    , Type.namedWith [] "Cons" [ Type.namedWith [] "Value" [] ]
                    )
                    valueTags.array
                , Elm.Case.branch3
                    "Closure"
                    ( "maybe", Type.namedWith [] "Maybe" [ Type.string ] )
                    ( "list.List", Type.list (Type.namedWith [] "Op" []) )
                    ( "environment", Type.namedWith [] "Environment" [] )
                    valueTags.closure
                ]
    , vM =
        \vMExpression vMTags ->
            Elm.Case.custom
                vMExpression
                (Type.namedWith [ "SECD", "VM" ] "VM" [])
                [ Elm.Case.branch5
                    "VM"
                    ( "context", Type.namedWith [] "Context" [] )
                    ( "stack", Type.namedWith [] "Stack" [] )
                    ( "environment", Type.namedWith [] "Environment" [] )
                    ( "control", Type.namedWith [] "Control" [] )
                    ( "dump", Type.namedWith [] "Dump" [] )
                    vMTags.vM
                ]
    }


call_ :
    { encodeValue : Elm.Expression -> Elm.Expression
    , encodeDump : Elm.Expression -> Elm.Expression
    , encodeControl : Elm.Expression -> Elm.Expression
    , encodeEnvironment : Elm.Expression -> Elm.Expression
    , encodeStack : Elm.Expression -> Elm.Expression
    , encodeContext : Elm.Expression -> Elm.Expression
    , encode : Elm.Expression -> Elm.Expression
    , addHoverable : Elm.Expression -> Elm.Expression
    , viewStackName : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewDumpVal : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewDump : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewControl : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewEnv : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewStack : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewValue : Elm.Expression -> Elm.Expression -> Elm.Expression
    , viewVMStack : Elm.Expression -> Elm.Expression
    , viewContext : Elm.Expression -> Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , getPages : Elm.Expression -> Elm.Expression -> Elm.Expression
    , evalPage :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , evalChunk : Elm.Expression -> Elm.Expression -> Elm.Expression
    , stepNAccumulate : Elm.Expression -> Elm.Expression -> Elm.Expression
    , evaluate : Elm.Expression -> Elm.Expression
    , recursiveApply : Elm.Expression -> Elm.Expression
    , returnFromFunction : Elm.Expression -> Elm.Expression
    , applyFunction : Elm.Expression -> Elm.Expression
    , loadFunction : Elm.Expression -> Elm.Expression
    , endIfElse : Elm.Expression -> Elm.Expression
    , evalIfElse : Elm.Expression -> Elm.Expression
    , evalUnary : Elm.Expression -> Elm.Expression -> Elm.Expression
    , evalBinary : Elm.Expression -> Elm.Expression -> Elm.Expression
    , evalFunc : Elm.Expression -> Elm.Expression -> Elm.Expression
    , locateInEnv :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , loadFromEnv : Elm.Expression -> Elm.Expression -> Elm.Expression
    , endProgram : Elm.Expression -> Elm.Expression -> Elm.Expression
    , step : Elm.Expression -> Elm.Expression
    , stepN : Elm.Expression -> Elm.Expression -> Elm.Expression
    , stateStep : Elm.Expression -> Elm.Expression
    , initState : Elm.Expression -> Elm.Expression
    , initRaw : Elm.Expression -> Elm.Expression
    , init : Elm.Expression -> Elm.Expression
    , valueToString : Elm.Expression -> Elm.Expression
    , vmNull : Elm.Expression -> Elm.Expression
    , vmCompare :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , vmCdr : Elm.Expression -> Elm.Expression
    , vmCar : Elm.Expression -> Elm.Expression
    , vmCons : Elm.Expression -> Elm.Expression -> Elm.Expression
    , vmAtom : Elm.Expression -> Elm.Expression
    , vmSub : Elm.Expression -> Elm.Expression -> Elm.Expression
    , vmMultiply : Elm.Expression -> Elm.Expression -> Elm.Expression
    , vmAdd : Elm.Expression -> Elm.Expression -> Elm.Expression
    , boolToValue : Elm.Expression -> Elm.Expression
    }
call_ =
    { encodeValue =
        \encodeValueArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "encodeValue"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" [] ]
                                (Type.namedWith [ "Encode" ] "Value" [])
                            )
                    }
                )
                [ encodeValueArg ]
    , encodeDump =
        \encodeDumpArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "encodeDump"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Dump" [] ]
                                (Type.namedWith [ "Encode" ] "Value" [])
                            )
                    }
                )
                [ encodeDumpArg ]
    , encodeControl =
        \encodeControlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "encodeControl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Control" [] ]
                                (Type.namedWith [ "Encode" ] "Value" [])
                            )
                    }
                )
                [ encodeControlArg ]
    , encodeEnvironment =
        \encodeEnvironmentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "encodeEnvironment"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Environment" [] ]
                                (Type.namedWith [ "Encode" ] "Value" [])
                            )
                    }
                )
                [ encodeEnvironmentArg ]
    , encodeStack =
        \encodeStackArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "encodeStack"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Stack" [] ]
                                (Type.namedWith [ "Encode" ] "Value" [])
                            )
                    }
                )
                [ encodeStackArg ]
    , encodeContext =
        \encodeContextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "encodeContext"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Context" [] ]
                                (Type.namedWith [ "Encode" ] "Value" [])
                            )
                    }
                )
                [ encodeContextArg ]
    , encode =
        \encodeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "encode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [ "Encode" ] "Value" [])
                            )
                    }
                )
                [ encodeArg ]
    , addHoverable =
        \addHoverableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "addHoverable"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        []
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.list
                                    (Type.namedWith
                                        []
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                )
                            )
                    }
                )
                [ addHoverableArg ]
    , viewStackName =
        \viewStackNameArg viewStackNameArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewStackName"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.list (Type.var "a") ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewStackNameArg, viewStackNameArg0 ]
    , viewDumpVal =
        \viewDumpValArg viewDumpValArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewDumpVal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ]
                                , Type.namedWith [] "DumpValue" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewDumpValArg, viewDumpValArg0 ]
    , viewDump =
        \viewDumpArg viewDumpArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewDump"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ]
                                , Type.namedWith [] "Dump" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewDumpArg, viewDumpArg0 ]
    , viewControl =
        \viewControlArg viewControlArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewControl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ]
                                , Type.namedWith [] "Control" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewControlArg, viewControlArg0 ]
    , viewEnv =
        \viewEnvArg viewEnvArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewEnv"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ]
                                , Type.namedWith [] "Environment" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewEnvArg, viewEnvArg0 ]
    , viewStack =
        \viewStackArg viewStackArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewStack"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ]
                                , Type.namedWith [] "Stack" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewStackArg, viewStackArg0 ]
    , viewValue =
        \viewValueArg viewValueArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewValue"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ]
                                , Type.namedWith [] "Value" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewValueArg, viewValueArg0 ]
    , viewVMStack =
        \viewVMStackArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewVMStack"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "n"
                                      , Type.namedWith [] "Maybe" [ Type.int ]
                                      )
                                    , ( "viewStackChunk"
                                      , Type.function
                                            [ Type.list (Type.var "a") ]
                                            (Type.list
                                                (Type.namedWith
                                                    []
                                                    "Element"
                                                    [ Type.var "msg" ]
                                                )
                                            )
                                      )
                                    , ( "stack", Type.list (Type.var "a") )
                                    , ( "stackName", Type.string )
                                    ]
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewVMStackArg ]
    , viewContext =
        \viewContextArg viewContextArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "viewContext"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Maybe" [ Type.int ]
                                , Type.namedWith [] "Context" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewContextArg, viewContextArg0 ]
    , view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "ViewOptions" []
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.namedWith [] "Element" [ Type.var "msg" ])
                            )
                    }
                )
                [ viewArg, viewArg0 ]
    , getPages =
        \getPagesArg getPagesArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "getPages"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "maxPages", Type.int )
                                    , ( "pageSize", Type.int )
                                    , ( "chunkSize", Type.int )
                                    ]
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.namedWith [] "PagesData" [])
                            )
                    }
                )
                [ getPagesArg, getPagesArg0 ]
    , evalPage =
        \evalPageArg evalPageArg0 evalPageArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "evalPage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.int
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.record
                                    [ ( "totalVMCount", Type.int )
                                    , ( "chunkVMs"
                                      , Type.list (Type.namedWith [] "VM" [])
                                      )
                                    , ( "result"
                                      , Type.namedWith
                                            []
                                            "Result"
                                            [ Type.namedWith [] "VM" []
                                            , Type.namedWith [] "VMResult" []
                                            ]
                                      )
                                    ]
                                )
                            )
                    }
                )
                [ evalPageArg, evalPageArg0, evalPageArg1 ]
    , evalChunk =
        \evalChunkArg evalChunkArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "evalChunk"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [], Type.int ]
                                (Type.tuple
                                    Type.int
                                    (Type.namedWith
                                        []
                                        "Result"
                                        [ Type.namedWith [] "VM" []
                                        , Type.namedWith [] "VMResult" []
                                        ]
                                    )
                                )
                            )
                    }
                )
                [ evalChunkArg, evalChunkArg0 ]
    , stepNAccumulate =
        \stepNAccumulateArg stepNAccumulateArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "stepNAccumulate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.namedWith [] "VM" [] ]
                                (Type.tuple
                                    (Type.list (Type.namedWith [] "VM" []))
                                    (Type.namedWith
                                        []
                                        "Result"
                                        [ Type.namedWith [] "VM" []
                                        , Type.namedWith [] "VMResult" []
                                        ]
                                    )
                                )
                            )
                    }
                )
                [ stepNAccumulateArg, stepNAccumulateArg0 ]
    , evaluate =
        \evaluateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "evaluate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.tuple
                                    Type.int
                                    (Type.namedWith [] "VMResult" [])
                                )
                            )
                    }
                )
                [ evaluateArg ]
    , recursiveApply =
        \recursiveApplyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "recursiveApply"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ recursiveApplyArg ]
    , returnFromFunction =
        \returnFromFunctionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "returnFromFunction"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ returnFromFunctionArg ]
    , applyFunction =
        \applyFunctionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "applyFunction"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ applyFunctionArg ]
    , loadFunction =
        \loadFunctionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "loadFunction"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ loadFunctionArg ]
    , endIfElse =
        \endIfElseArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "endIfElse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ endIfElseArg ]
    , evalIfElse =
        \evalIfElseArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "evalIfElse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ evalIfElseArg ]
    , evalUnary =
        \evalUnaryArg evalUnaryArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "evalUnary"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.namedWith [] "Value" [] ]
                                    (Type.namedWith
                                        []
                                        "Result"
                                        [ Type.string
                                        , Type.namedWith [] "Value" []
                                        ]
                                    )
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ evalUnaryArg, evalUnaryArg0 ]
    , evalBinary =
        \evalBinaryArg evalBinaryArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "evalBinary"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.namedWith [] "Value" []
                                    , Type.namedWith [] "Value" []
                                    ]
                                    (Type.namedWith
                                        []
                                        "Result"
                                        [ Type.string
                                        , Type.namedWith [] "Value" []
                                        ]
                                    )
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ evalBinaryArg, evalBinaryArg0 ]
    , evalFunc =
        \evalFuncArg evalFuncArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "evalFunc"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Func" []
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ evalFuncArg, evalFuncArg0 ]
    , locateInEnv =
        \locateInEnvArg locateInEnvArg0 locateInEnvArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "locateInEnv"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.tuple Type.int Type.int
                                , Type.namedWith [] "Environment" []
                                , Type.namedWith [] "Context" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith
                                        []
                                        "Cons"
                                        [ Type.namedWith [] "Value" [] ]
                                    ]
                                )
                            )
                    }
                )
                [ locateInEnvArg, locateInEnvArg0, locateInEnvArg1 ]
    , loadFromEnv =
        \loadFromEnvArg loadFromEnvArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "loadFromEnv"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.tuple Type.int Type.int
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ loadFromEnvArg, loadFromEnvArg0 ]
    , endProgram =
        \endProgramArg endProgramArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "endProgram"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Stack" []
                                , Type.namedWith [] "VM" []
                                ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ endProgramArg, endProgramArg0 ]
    , step =
        \stepArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "step"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ stepArg ]
    , stepN =
        \stepNArg stepNArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "stepN"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ stepNArg, stepNArg0 ]
    , stateStep =
        \stateStepArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "stateStep"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "State" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ stateStepArg ]
    , initState =
        \initStateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "initState"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "VM" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ initStateArg ]
    , initRaw =
        \initRawArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "initRaw"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Program" [] ]
                                (Type.namedWith [] "VM" [])
                            )
                    }
                )
                [ initRawArg ]
    , init =
        \initArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "init"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Program" [] ]
                                (Type.namedWith [] "State" [])
                            )
                    }
                )
                [ initArg ]
    , valueToString =
        \valueToStringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "valueToString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" [] ]
                                Type.string
                            )
                    }
                )
                [ valueToStringArg ]
    , vmNull =
        \vmNullArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmNull"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" [] ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmNullArg ]
    , vmCompare =
        \vmCompareArg vmCompareArg0 vmCompareArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmCompare"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Cmp" []
                                , Type.namedWith [] "Value" []
                                , Type.namedWith [] "Value" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmCompareArg, vmCompareArg0, vmCompareArg1 ]
    , vmCdr =
        \vmCdrArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmCdr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" [] ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmCdrArg ]
    , vmCar =
        \vmCarArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmCar"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" [] ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmCarArg ]
    , vmCons =
        \vmConsArg vmConsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmCons"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" []
                                , Type.namedWith [] "Value" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmConsArg, vmConsArg0 ]
    , vmAtom =
        \vmAtomArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmAtom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" [] ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmAtomArg ]
    , vmSub =
        \vmSubArg vmSubArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmSub"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" []
                                , Type.namedWith [] "Value" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmSubArg, vmSubArg0 ]
    , vmMultiply =
        \vmMultiplyArg vmMultiplyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmMultiply"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" []
                                , Type.namedWith [] "Value" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmMultiplyArg, vmMultiplyArg0 ]
    , vmAdd =
        \vmAddArg vmAddArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "vmAdd"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Value" []
                                , Type.namedWith [] "Value" []
                                ]
                                (Type.namedWith
                                    []
                                    "Result"
                                    [ Type.string
                                    , Type.namedWith [] "Value" []
                                    ]
                                )
                            )
                    }
                )
                [ vmAddArg, vmAddArg0 ]
    , boolToValue =
        \boolToValueArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "SECD", "VM" ]
                    , name = "boolToValue"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith [] "Value" [])
                            )
                    }
                )
                [ boolToValueArg ]
    }


values_ :
    { valueDecoder : Elm.Expression
    , encodeValue : Elm.Expression
    , dumpDecoder : Elm.Expression
    , encodeDump : Elm.Expression
    , controlDecoder : Elm.Expression
    , encodeControl : Elm.Expression
    , environmentDecoder : Elm.Expression
    , encodeEnvironment : Elm.Expression
    , stackDecoder : Elm.Expression
    , encodeStack : Elm.Expression
    , contextDecoder : Elm.Expression
    , encodeContext : Elm.Expression
    , decoder : Elm.Expression
    , encode : Elm.Expression
    , addHoverable : Elm.Expression
    , viewStackName : Elm.Expression
    , viewDumpVal : Elm.Expression
    , viewDump : Elm.Expression
    , viewControl : Elm.Expression
    , viewEnv : Elm.Expression
    , viewStack : Elm.Expression
    , viewValue : Elm.Expression
    , viewVMStack : Elm.Expression
    , viewContext : Elm.Expression
    , view : Elm.Expression
    , getPages : Elm.Expression
    , evalPage : Elm.Expression
    , evalChunk : Elm.Expression
    , stepNAccumulate : Elm.Expression
    , evaluate : Elm.Expression
    , recursiveApply : Elm.Expression
    , returnFromFunction : Elm.Expression
    , applyFunction : Elm.Expression
    , loadFunction : Elm.Expression
    , endIfElse : Elm.Expression
    , evalIfElse : Elm.Expression
    , evalUnary : Elm.Expression
    , evalBinary : Elm.Expression
    , evalFunc : Elm.Expression
    , locateInEnv : Elm.Expression
    , loadFromEnv : Elm.Expression
    , endProgram : Elm.Expression
    , step : Elm.Expression
    , stepN : Elm.Expression
    , stateStep : Elm.Expression
    , initState : Elm.Expression
    , initRaw : Elm.Expression
    , init : Elm.Expression
    , valueToString : Elm.Expression
    , vmNull : Elm.Expression
    , vmCompare : Elm.Expression
    , vmCdr : Elm.Expression
    , vmCar : Elm.Expression
    , vmCons : Elm.Expression
    , vmAtom : Elm.Expression
    , vmSub : Elm.Expression
    , vmMultiply : Elm.Expression
    , vmAdd : Elm.Expression
    , nil : Elm.Expression
    , boolToValue : Elm.Expression
    , initCtx : Elm.Expression
    }
values_ =
    { valueDecoder =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "valueDecoder"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Decoder"
                        [ Type.namedWith [] "Value" [] ]
                    )
            }
    , encodeValue =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeValue"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
    , dumpDecoder =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "dumpDecoder"
            , annotation =
                Just
                    (Type.namedWith [] "Decoder" [ Type.namedWith [] "Dump" [] ]
                    )
            }
    , encodeDump =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeDump"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Dump" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
    , controlDecoder =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "controlDecoder"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Decoder"
                        [ Type.namedWith [] "Control" [] ]
                    )
            }
    , encodeControl =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeControl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Control" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
    , environmentDecoder =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "environmentDecoder"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Decoder"
                        [ Type.namedWith [] "Environment" [] ]
                    )
            }
    , encodeEnvironment =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeEnvironment"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Environment" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
    , stackDecoder =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "stackDecoder"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Decoder"
                        [ Type.namedWith [] "Stack" [] ]
                    )
            }
    , encodeStack =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeStack"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Stack" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
    , contextDecoder =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "contextDecoder"
            , annotation =
                Just
                    (Type.namedWith
                        []
                        "Decoder"
                        [ Type.namedWith [] "Context" [] ]
                    )
            }
    , encodeContext =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encodeContext"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Context" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
    , decoder =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "decoder"
            , annotation =
                Just (Type.namedWith [] "Decoder" [ Type.namedWith [] "VM" [] ])
            }
    , encode =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "encode"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [ "Encode" ] "Value" [])
                    )
            }
    , addHoverable =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "addHoverable"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        ]
                        (Type.list
                            (Type.namedWith [] "Attribute" [ Type.var "msg" ])
                        )
                    )
            }
    , viewStackName =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewStackName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.list (Type.var "a") ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewDumpVal =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewDumpVal"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "DumpValue" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewDump =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewDump"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Dump" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewControl =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewControl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Control" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewEnv =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewEnv"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Environment" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewStack =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewStack"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Stack" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewValue =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewValue"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewVMStack =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewVMStack"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "n", Type.namedWith [] "Maybe" [ Type.int ] )
                            , ( "viewStackChunk"
                              , Type.function
                                    [ Type.list (Type.var "a") ]
                                    (Type.list
                                        (Type.namedWith
                                            []
                                            "Element"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "stack", Type.list (Type.var "a") )
                            , ( "stackName", Type.string )
                            ]
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , viewContext =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "viewContext"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Maybe" [ Type.int ]
                        , Type.namedWith [] "Context" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , view =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "ViewOptions" []
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "Element" [ Type.var "msg" ])
                    )
            }
    , getPages =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "getPages"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "maxPages", Type.int )
                            , ( "pageSize", Type.int )
                            , ( "chunkSize", Type.int )
                            ]
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "PagesData" [])
                    )
            }
    , evalPage =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalPage"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int, Type.namedWith [] "VM" [] ]
                        (Type.record
                            [ ( "totalVMCount", Type.int )
                            , ( "chunkVMs"
                              , Type.list (Type.namedWith [] "VM" [])
                              )
                            , ( "result"
                              , Type.namedWith
                                    []
                                    "Result"
                                    [ Type.namedWith [] "VM" []
                                    , Type.namedWith [] "VMResult" []
                                    ]
                              )
                            ]
                        )
                    )
            }
    , evalChunk =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalChunk"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [], Type.int ]
                        (Type.tuple
                            Type.int
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.namedWith [] "VM" []
                                , Type.namedWith [] "VMResult" []
                                ]
                            )
                        )
                    )
            }
    , stepNAccumulate =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "stepNAccumulate"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [] "VM" [] ]
                        (Type.tuple
                            (Type.list (Type.namedWith [] "VM" []))
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.namedWith [] "VM" []
                                , Type.namedWith [] "VMResult" []
                                ]
                            )
                        )
                    )
            }
    , evaluate =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evaluate"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.tuple Type.int (Type.namedWith [] "VMResult" []))
                    )
            }
    , recursiveApply =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "recursiveApply"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , returnFromFunction =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "returnFromFunction"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , applyFunction =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "applyFunction"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , loadFunction =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "loadFunction"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , endIfElse =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "endIfElse"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , evalIfElse =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalIfElse"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , evalUnary =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalUnary"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith [] "Value" [] ]
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.string, Type.namedWith [] "Value" [] ]
                            )
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , evalBinary =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalBinary"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith [] "Value" []
                            , Type.namedWith [] "Value" []
                            ]
                            (Type.namedWith
                                []
                                "Result"
                                [ Type.string, Type.namedWith [] "Value" [] ]
                            )
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , evalFunc =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "evalFunc"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Func" []
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , locateInEnv =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "locateInEnv"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple Type.int Type.int
                        , Type.namedWith [] "Environment" []
                        , Type.namedWith [] "Context" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string
                            , Type.namedWith
                                []
                                "Cons"
                                [ Type.namedWith [] "Value" [] ]
                            ]
                        )
                    )
            }
    , loadFromEnv =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "loadFromEnv"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple Type.int Type.int
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , endProgram =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "endProgram"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Stack" []
                        , Type.namedWith [] "VM" []
                        ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , step =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "step"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , stepN =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "stepN"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , stateStep =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "stateStep"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "State" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , initState =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "initState"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "VM" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , initRaw =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "initRaw"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.namedWith [] "VM" [])
                    )
            }
    , init =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "init"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Program" [] ]
                        (Type.namedWith [] "State" [])
                    )
            }
    , valueToString =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "valueToString"
            , annotation =
                Just
                    (Type.function [ Type.namedWith [] "Value" [] ] Type.string)
            }
    , vmNull =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmNull"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmCompare =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCompare"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Cmp" []
                        , Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmCdr =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCdr"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmCar =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCar"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmCons =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmCons"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmAtom =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmAtom"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" [] ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmSub =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmSub"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmMultiply =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmMultiply"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , vmAdd =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "vmAdd"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Value" []
                        , Type.namedWith [] "Value" []
                        ]
                        (Type.namedWith
                            []
                            "Result"
                            [ Type.string, Type.namedWith [] "Value" [] ]
                        )
                    )
            }
    , nil =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "nil"
            , annotation = Just (Type.namedWith [] "Value" [])
            }
    , boolToValue =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "boolToValue"
            , annotation =
                Just
                    (Type.function [ Type.bool ] (Type.namedWith [] "Value" []))
            }
    , initCtx =
        Elm.value
            { importFrom = [ "SECD", "VM" ]
            , name = "initCtx"
            , annotation = Just (Type.namedWith [] "Context" [])
            }
    }


