module SECD.VM exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Lib.Cons as Cons exposing (Cons)
import SECD.Error as Error exposing (Error)
import SECD.Program as Program exposing (Cmp, Func(..), Op(..), Program)
import SECD.VMEnv as Env exposing (EnvItem(..))



-- | SECD Virtual Machine
-- | Runs an SECD program
---- Types ----


type VM
    = VM
        -- Context (see below)
        Context
        -- Stack used for evaluation of expressions
        Stack
        --  Environment used to store the current value list
        Environment
        -- Control used to store the current instruction pointer
        Control
        -- Dump to store anything else
        Dump



-- context (right now), just stores the values of the Dummy value (if possible)
-- the Dummy value is a pointer to a value, which RAP changes to the current function scope (i think)


type alias Context =
    { dummyVal : Maybe (List (Cons Value)) }


initCtx : Context
initCtx =
    { dummyVal = Nothing }



-- Dump stack used to store the current state of the VM


type alias Stack =
    List Value


type alias Environment =
    Env.Environment Value


type alias Control =
    List Op


type alias Dump =
    List DumpValue



-- types of values that can be in the stack


type Value
    = Integer Int
    | Truthy -- false is represented by Nil
    | Array (Cons Value)
    | Closure (List Op) Environment -- When loading a function, dump the closure of the function (i think?)


boolToValue : Bool -> Value
boolToValue b =
    if b then
        Truthy

    else
        nil



-- for brevity, we merge the Nil pointer with the Cons array definition


nil : Value
nil =
    Array Cons.Nil



-- Elm functions acting on the values


vmAdd : Value -> Value -> Result String Value
vmAdd va vb =
    case ( va, vb ) of
        ( Integer a, Integer b ) ->
            Ok <| Integer (a + b)

        _ ->
            Err <| "add: Expecting two integers, got " ++ valueToString va ++ " and " ++ valueToString vb


vmMultiply : Value -> Value -> Result String Value
vmMultiply va vb =
    case ( va, vb ) of
        ( Integer a, Integer b ) ->
            Ok <| Integer (a * b)

        _ ->
            Err <| "multiply: Expecting two integers, got " ++ valueToString va ++ " and " ++ valueToString vb


vmSub : Value -> Value -> Result String Value
vmSub va vb =
    case ( va, vb ) of
        ( Integer a, Integer b ) ->
            Ok <| Integer (a - b)

        _ ->
            Err <| "subtract: Expecting two integers, got " ++ valueToString va ++ " and " ++ valueToString vb


vmAtom : Value -> Result String Value
vmAtom v =
    case v of
        Truthy ->
            Ok Truthy

        Integer _ ->
            Ok Truthy

        -- nil is the only thing that is both an atom and a list
        Array Cons.Nil ->
            Ok Truthy

        Array _ ->
            Ok nil

        Closure _ _ ->
            Ok nil



-- joins a head and tail list


vmCons : Value -> Value -> Result String Value
vmCons head tail =
    case ( head, tail ) of
        ( Array ch, Array ct ) ->
            Ok <| Array (Cons.Cons ch ct)

        ( h, Array ct ) ->
            Ok <| Array (Cons.cons (Cons.single h) ct)

        ( h, t ) ->
            Ok <| Array (Cons.cons (Cons.single h) (Cons.single t))


vmCar : Value -> Result String Value
vmCar v =
    case v of
        Array (Cons.Cons (Cons.Val a) _) ->
            Ok a

        Array (Cons.Cons Cons.Nil _) ->
            Ok nil

        Array (Cons.Cons a _) ->
            Ok <| Array a

        Array Cons.Nil ->
            Ok <| nil

        -- nil is the only thing that is both an atom and a list
        _ ->
            Err <| "car: Expecting an array, got " ++ valueToString v


vmCdr : Value -> Result String Value
vmCdr v =
    case v of
        Array (Cons.Cons _ (Cons.Val a)) ->
            Ok a

        Array (Cons.Cons _ a) ->
            Ok <| Array a

        -- this is honestly just not right. But lisp does this anyway..
        Array Cons.Nil ->
            Ok nil

        -- nil is the only thing that is both an atom and a list
        _ ->
            Err <| "cdr: Expecting an array, got " ++ valueToString v


vmCompare : Cmp -> Value -> Value -> Result String Value
vmCompare cmp va vb =
    case ( va, vb ) of
        ( Integer a, Integer b ) ->
            Ok <| boolToValue <| Program.cmpFunc cmp a b

        _ ->
            Err <| "compare: Expecting two integers, got " ++ valueToString va ++ " and " ++ valueToString vb


vmNull : Value -> Result String Value
vmNull v =
    case v of
        Array Cons.Nil ->
            Ok Truthy

        _ ->
            Ok nil


valueToString : Value -> String
valueToString val =
    case val of
        Integer i ->
            String.fromInt i

        Truthy ->
            "true"

        Array arr ->
            Cons.toString valueToString arr

        Closure _ _ ->
            "Closure"


viewValue : Value -> Html msg
viewValue val =
    case val of
        Integer i ->
            Html.div [ Attr.class "vm-val vm-int" ] [ Html.text (String.fromInt i) ]

        Truthy ->
            Html.div [ Attr.class "vm-val vm-truthy" ] [ Html.text "true" ]

        Array arr ->
            Html.div [ Attr.class "vm-val row vm-array" ] [ Cons.view viewValue arr ]

        Closure f env ->
            Html.div
                [ Attr.class "vm-val row complex vm-closure" ]
                [ Html.text "Closure"
                , Html.div [ Attr.class "vm-closure-func" ] [ viewControl f ]
                , Html.div [ Attr.class "vm-closure-env" ] [ Env.view viewValue env ]
                ]



-- types of values we can store in the dump stack


type DumpValue
    = Control (List Op) -- in Sel, we want to dump the contents of the control stack
    | EntireState Stack Environment Control -- When applying a function, we want to dump the state of the VM



---- VM Operations ----


init : Program -> State
init prog =
    Unfinished <| initRaw prog


initRaw : Program -> VM
initRaw prog =
    VM initCtx [] Env.init (Program.toList prog) []



-- | Evaluate a program


type State
    = Unfinished VM
    | Finished VM Value
    | Error VM String


initState : VM -> State
initState =
    Unfinished


stateStep : State -> State
stateStep st =
    case st of
        Unfinished vm ->
            step vm

        Finished _ _ ->
            st

        Error _ _ ->
            st



-- take max N steps, returning the final state
-- mostly used in tests


stepN : Int -> VM -> State
stepN n vm =
    if n <= 0 then
        Unfinished vm

    else
        case step vm of
            Unfinished vm_ ->
                stepN (n - 1) vm_

            _ ->
                -- return "finished" value
                step vm


step : VM -> State
step vm =
    let
        (VM ctx s e c d) =
            vm
    in
    case c of
        [] ->
            endProgram s vm

        -- evaluate Nil
        NIL :: c_ ->
            Unfinished (VM ctx (nil :: s) e c_ d)

        -- evaluate Load Constant
        (LDC x) :: c_ ->
            Unfinished (VM ctx (Integer x :: s) e c_ d)

        -- evaluate load variable
        (LD ( x, y )) :: c_ ->
            loadFromEnv ( x, y ) (VM ctx s e c_ d)

        -- Binary or unary operator
        (FUNC f) :: c_ ->
            evalFunc f (VM ctx s e c_ d)

        -- IF/THEN/ELSE
        SEL :: c_ ->
            evalIfElse (VM ctx s e c_ d)

        JOIN :: c_ ->
            endIfElse (VM ctx s e c_ d)

        -- Load and evaluating functions
        LDF :: c_ ->
            loadFunction (VM ctx s e c_ d)

        AP :: c_ ->
            applyFunction (VM ctx s e c_ d)

        RTN :: c_ ->
            returnFromFunction (VM ctx s e c_ d)

        -- Recursive functions
        DUM :: c_ ->
            Unfinished (VM ctx s (Env.pushDummy e) c_ d)

        RAP :: c_ ->
            recursiveApply (VM ctx s e c_ d)

        _ ->
            Error vm "VM: step: VM is in an invalid state"



-- finished an evaluation when the Control stack is empty


endProgram : Stack -> VM -> State
endProgram stack vm =
    case stack of
        [] ->
            Error vm "Stack is empty on program exit"

        val :: _ ->
            Finished vm val


loadFromEnv : ( Int, Int ) -> VM -> State
loadFromEnv ( x, y ) (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case Env.locate ( x, y ) ctx.dummyVal e of
        Err errStr ->
            Error vm <| "VM: loadFromEnv: Variable not found from coords (" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")\n" ++ errStr

        Ok (Cons.Val a) ->
            Unfinished (VM ctx (a :: s) e c d)

        Ok consArr ->
            Unfinished (VM ctx (Array consArr :: s) e c d)


evalFunc : Func -> VM -> State
evalFunc f vm =
    case f of
        ADD ->
            evalBinary vmAdd vm

        MULT ->
            evalBinary vmMultiply vm

        SUB ->
            evalBinary vmSub vm

        ATOM ->
            evalUnary vmAtom vm

        CONS ->
            evalBinary vmCons vm

        CAR ->
            evalUnary vmCar vm

        CDR ->
            evalUnary vmCdr vm

        NULL ->
            evalUnary vmNull vm

        COMPARE cmp ->
            evalBinary (vmCompare cmp) vm


evalBinary : (Value -> Value -> Result String Value) -> VM -> State
evalBinary op (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case s of
        a :: b :: s_ ->
            case op a b of
                Ok val ->
                    Unfinished (VM ctx (val :: s_) e c d)

                Err msg ->
                    Error vm ("VM: evalBinary: " ++ msg)

        _ ->
            Error vm <|
                "VM: evalBinary: Stack of size "
                    ++ (String.fromInt <| List.length s)
                    ++ " does not have two elements!"


evalUnary : (Value -> Result String Value) -> VM -> State
evalUnary op (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case s of
        a :: s_ ->
            case op a of
                Ok val ->
                    Unfinished (VM ctx (val :: s_) e c d)

                Err msg ->
                    Error vm ("VM: evalUnary: " ++ msg)

        _ ->
            Error vm <|
                "VM: evalUnary: Stack of size "
                    ++ (String.fromInt <| List.length s)
                    ++ " is empty!"


evalIfElse : VM -> State
evalIfElse (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case ( s, c ) of
        ( [], _ ) ->
            Error vm "VM: evalIfElse: Stack is empty! Expecting boolean value"

        ( Truthy :: s_, (NESTED ct) :: (NESTED _) :: c_ ) ->
            let
                -- update dump value to hold the control stack after the if/else
                newD =
                    Control c_ :: d
            in
            Unfinished (VM ctx s_ e ct newD)

        ( (Array Cons.Nil) :: s_, (NESTED _) :: (NESTED cf) :: c_ ) ->
            let
                -- update dump value to hold the control stack after the if/else
                newD =
                    Control c_ :: d
            in
            Unfinished (VM ctx s_ e cf newD)

        ( a :: _, _ ) ->
            case a of
                Truthy ->
                    Error vm "VM: evalIfElse: Failure to complete if/else: Control stack is not Nested (Can't find case branch)"

                Array Cons.Nil ->
                    Error vm "VM: evalIfElse: Failure to complete if/else: Control stack is not Nested (Can't find case branches)"

                _ ->
                    Error vm <| "VM: evalIfElse: Expected boolean-ish value, got " ++ valueToString a


endIfElse : VM -> State
endIfElse (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case d of
        (Control c_) :: d_ ->
            Unfinished (VM ctx s e c_ d_)

        _ ->
            Error vm "VM: endIfElse: Dump stack does not hold control values"


loadFunction : VM -> State
loadFunction (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case c of
        (NESTED funcBody) :: c_ ->
            Unfinished (VM ctx (Closure funcBody e :: s) e c_ d)

        _ ->
            Error vm "VM: loadFunction: cannot find function body!"


applyFunction : VM -> State
applyFunction (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case s of
        -- vals is a list of the values of the arguments
        -- if there are no arguments, such as `lambda () ...`, then vals is Nil
        (Closure funcBody env) :: (Array vals) :: s_ ->
            case Cons.toList vals of
                Nothing ->
                    Error vm "VM: applyFunction: badly formatted closure!"

                Just values ->
                    Unfinished (VM ctx [] (Env.push values env) funcBody (EntireState s_ e c :: d))

        _ ->
            Error vm "VM: applyFunction: cannot find function body!"


returnFromFunction : VM -> State
returnFromFunction (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case ( s, d ) of
        ( x :: _, (EntireState s_ e_ c_) :: d_ ) ->
            Unfinished (VM ctx (x :: s_) e_ c_ d_)

        ( _, (EntireState _ _ _) :: _ ) ->
            Error vm "VM: returnFromFunction: Cannot find function return value!"

        _ ->
            Error vm "VM: returnFromFunction: Dump stack does not hold correct values!"


recursiveApply : VM -> State
recursiveApply (VM ctx s e c d) =
    let
        vm =
            VM ctx s e c d
    in
    case ( s, e ) of
        ( (Closure funcBody env) :: (Array vals) :: s_, Env.Dummy :: e_ ) ->
            case Cons.toList vals of
                Nothing ->
                    Error vm "VM: recursiveApply: badly formatted closure!"

                Just values ->
                    case Env.replaceDummy values env of
                        Ok newEnv ->
                            Unfinished (VM { ctx | dummyVal = Just values } [] newEnv funcBody (EntireState s_ e_ c :: d))

                        Err errStr ->
                            Error vm ("VM: recursiveApply: " ++ errStr)

        _ ->
            Error vm "VM: recursiveApply: Cannot find function body!"



-- EVALUATION


type alias VMResult =
    Result Error Value



-- fully evaluate a VM
-- WARNING: this will blow up the stack if it goes in an infinite loop
-- this is mainly used for testing


evaluate : VM -> VMResult
evaluate oldVm =
    case step oldVm of
        Unfinished vm ->
            evaluate vm

        Finished _ val ->
            Ok val

        Error _ err ->
            Err <| err



-- fully evaluate a VM, keeping track of all states


evalList : VM -> List State
evalList oldVm =
    case step oldVm of
        Unfinished vm ->
            Unfinished vm :: evalList vm

        other ->
            List.singleton other


{-|


## evalChunk

evaluate a VM chunk, returning the state/value, and first VM state in that chunk

Note that, compared to evalN, evalChunk only keeps the first VM state

  - If we're not finished, return `(startVM, Err nextVM)`
  - If we're finished, return `(startVM, Ok (vmResult, count))`

**Note**: initial state is just the VM that was passed in

`nextVM` is the vm you would want to start off of for the next chunk.

Running `evalN startVM n` will give you a list of VMs from `startVM + 1` to `nextVM - 1`.

-}
evalChunk : VM -> Int -> ( VM, Result VM ( VMResult, Int ) )
evalChunk vm chunkSize =
    let
        helper : Int -> VM -> Result VM ( VMResult, Int )
        helper n_ vm_ =
            case step vm_ of
                Unfinished newVm ->
                    if n_ == 0 then
                        Err newVm

                    else
                        helper (n_ - 1) newVm

                Finished _ val ->
                    Ok ( Ok val, n_ )

                Error _ err ->
                    Ok ( Err err, n_ )
    in
    ( vm, helper chunkSize vm )


{-|


## evalN

evaluate a VM a max of `chunkSize` times, returning all states in that range

  - If we're not finished, return `Err (currState, allStates)`
  - If we're finished, return `Ok (returnValue, allStates)`

**Note**: we do not include the initial VM in the list of VM states

The VM returned if the program is not finished execuing is also not included in the list of VM states

-}
evalN : VM -> Int -> ( List VM, Result VM VMResult )
evalN vm chunkSize =
    let
        helper : Int -> VM -> List VM -> ( List VM, Result VM VMResult )
        helper n vm_ states =
            case step vm_ of
                Unfinished newVm ->
                    if n == 0 then
                        ( List.reverse states, Err newVm )

                    else
                        helper (n - 1) newVm (newVm :: states)

                Finished _ val ->
                    ( states, Ok <| Ok val )

                Error _ err ->
                    ( states, Ok (Err err) )
    in
    helper chunkSize vm []


{-|


## evalPage

evaluate a PAGE of VM states, or a list of chunks
each page can holds `pageSize` chunks, and each chunk holds `chunkSize` states
We accumulate the first VM state of each chunk

  - if we're not finished, return `(totalVMs, chunkVMs, Err lastVMState)`
    0 If we're finished, return `(totalVMs, chunkVMs, Ok vmResult)`

**Note** totalVMs is the count for _ALL_ vm states

-}
evalPage : VM -> Int -> Int -> { totalVMCount : Int, chunkVMs : List VM, result : Result VM VMResult }
evalPage vm pageSize chunkSize =
    let
        helper : VM -> Int -> Int -> List VM -> ( Int, List VM, Result VM VMResult )
        helper startVM vmCount pageCount stateAcc =
            if pageCount == pageSize then
                ( vmCount, stateAcc, Err startVM )

            else
                -- chunkVM is the first VM state of the chunk
                case evalChunk startVM chunkSize of
                    ( chunkVM, Ok ( val, vmChunkCount ) ) ->
                        -- evaluation finished
                        ( vmCount + vmChunkCount, chunkVM :: stateAcc, Ok val )

                    ( chunkVM, Err nextVM ) ->
                        helper nextVM (vmCount + chunkSize) (pageCount + 1) (chunkVM :: stateAcc)

        ( totalVMCount, chunkVMs, result ) =
            helper vm 0 0 []
    in
    { totalVMCount = totalVMCount, chunkVMs = List.reverse chunkVMs, result = result }



-- DEBUG


viewState : Int -> State -> Html msg
viewState lookahead st =
    let
        ( title, err, body ) =
            case st of
                Unfinished vm ->
                    ( "Unfinished", Html.text "", viewVM lookahead vm )

                Finished vm val ->
                    ( "Finished: " ++ valueToString val, Html.text "", viewVM lookahead vm )

                Error vm e ->
                    ( "Error", Error.view e, viewVM lookahead vm )
    in
    Html.div
        [ Attr.class "state" ]
        [ Html.h3 [ Attr.class "state-title" ] [ Html.text title ]
        , Html.div [ Attr.class "state-error" ] [ err ]
        , Html.div [ Attr.class "state-body" ] [ body ]
        ]


viewVM : Int -> VM -> Html msg
viewVM _ (VM ctx s e c d) =
    Html.div
        [ Attr.class "vm" ]
        [ Html.p [ Attr.class "vm-title ctx-title" ] [ Html.text "Context" ]
        , viewContext ctx
        , Html.p [ Attr.class "vm-title stack-title" ] [ Html.text "Stack" ]
        , viewStack s
        , Html.p [ Attr.class "vm-title env-title" ] [ Html.text "Environment" ]
        , Env.view viewValue e
        , Html.p [ Attr.class "vm-title control-title" ] [ Html.text "Control" ]
        , viewControl c
        , Html.p [ Attr.class "vm-title dump-title" ] [ Html.text "Dump" ]
        , viewDump d
        ]


viewContext : Context -> Html msg
viewContext ctx =
    let
        dummyVal =
            case ctx.dummyVal of
                Nothing ->
                    Html.text "Nothing"

                Just v ->
                    List.map (Cons.view viewValue) v
                        |> List.intersperse (Html.text ",")
                        |> Html.div [ Attr.class "dummy-val" ]
    in
    Html.div [ Attr.class "vm-body row ctx-body" ]
        [ Html.text "dummyVal = ", dummyVal ]


viewStack : Stack -> Html msg
viewStack s =
    let
        stackElems =
            List.map viewValue s
                |> List.intersperse (Html.text ",")
    in
    Html.div
        [ Attr.class "vm-body row stack-body" ]
        stackElems


viewControl : Control -> Html msg
viewControl c =
    let
        controlElems =
            List.map Program.view c
                |> List.intersperse (Html.text ",")
    in
    Html.div
        [ Attr.class "vm-body row control-body" ]
        controlElems


viewDump : Dump -> Html msg
viewDump d =
    Html.div
        [ Attr.class "vm-body row dump-body" ]
        (List.map viewDumpVal d)


viewDumpVal : DumpValue -> Html msg
viewDumpVal dv =
    case dv of
        Control c ->
            Html.div
                [ Attr.class "row dump-val dump-control" ]
                [ Html.text "Control"
                , viewControl c
                ]

        EntireState s e c ->
            Html.div
                [ Attr.class "row dump-val dump-entire-state" ]
                [ Html.text "EntireState"
                , viewStack s
                , Env.view viewValue e
                , viewControl c
                ]



---- ENCODERS/DECODERS ----


encode : VM -> Encode.Value
encode (VM ctx s e c d) =
    Encode.object
        [ ( "ctx", encodeContext ctx )
        , ( "s", encodeStack s )
        , ( "e", encodeEnvironment e )
        , ( "c", encodeControl c )
        , ( "d", encodeDump d )
        ]


decoder : Decoder VM
decoder =
    Decode.map5 VM
        (Decode.field "ctx" contextDecoder)
        (Decode.field "s" stackDecoder)
        (Decode.field "e" environmentDecoder)
        (Decode.field "c" controlDecoder)
        (Decode.field "d" dumpDecoder)



-- CONTEXT


encodeContext : Context -> Encode.Value
encodeContext ctx =
    let
        encodedDummyVal =
            case ctx.dummyVal of
                Nothing ->
                    Encode.null

                Just v ->
                    Encode.list (Cons.encode encodeValue) v
    in
    Encode.object
        [ ( "dmy", encodedDummyVal ) ]


contextDecoder : Decoder Context
contextDecoder =
    let
        dummyValDecoder =
            Decode.oneOf
                [ Decode.null Nothing
                , Decode.map Just <| Decode.list (Cons.decoder valueDecoder)
                ]
    in
    Decode.map Context
        (Decode.field "dmy" dummyValDecoder)



-- STACK
-- just a list of values


encodeStack : Stack -> Encode.Value
encodeStack =
    Encode.list encodeValue


stackDecoder : Decoder Stack
stackDecoder =
    Decode.list valueDecoder



-- ENVIRONMENT


encodeEnvironment : Environment -> Encode.Value
encodeEnvironment =
    Env.encode encodeValue


environmentDecoder : Decoder Environment
environmentDecoder =
    Env.decoder valueDecoder



-- CONTROL


encodeControl : Control -> Encode.Value
encodeControl =
    Encode.list Program.encodeSingle


controlDecoder : Decoder Control
controlDecoder =
    Decode.list Program.singleDecoder



-- DUMP


encodeDump : Dump -> Encode.Value
encodeDump =
    let
        encodeDumpVal : DumpValue -> Encode.Value
        encodeDumpVal dv =
            case dv of
                Control c ->
                    Encode.object
                        [ ( "t", Encode.string "C" )
                        , ( "v", encodeControl c )
                        ]

                EntireState s e c ->
                    Encode.object
                        [ ( "t", Encode.string "E" )
                        , ( "s", encodeStack s )
                        , ( "e", encodeEnvironment e )
                        , ( "c", encodeControl c )
                        ]
    in
    Encode.list encodeDumpVal


dumpDecoder : Decoder Dump
dumpDecoder =
    let
        dumpValueDecoder =
            Decode.field "t" Decode.string
                |> Decode.andThen
                    (\tag ->
                        case tag of
                            "C" ->
                                Decode.map Control <| Decode.field "v" controlDecoder

                            "E" ->
                                Decode.map3 EntireState
                                    (Decode.field "s" stackDecoder)
                                    (Decode.field "e" environmentDecoder)
                                    (Decode.field "c" controlDecoder)

                            _ ->
                                Decode.fail <| "Parsing dump: Invalid tag: " ++ tag
                    )
    in
    Decode.list dumpValueDecoder



-- VALUE


encodeValue : Value -> Encode.Value
encodeValue v =
    case v of
        Integer n ->
            Encode.object
                [ ( "t", Encode.string "n" )
                , ( "v", Encode.int n )
                ]

        Truthy ->
            Encode.object
                [ ( "t", Encode.string "t" ) ]

        Array cons ->
            Encode.object
                [ ( "t", Encode.string "a" )
                , ( "v", Cons.encode encodeValue cons )
                ]

        Closure ops env ->
            Encode.object
                [ ( "t", Encode.string "c" )
                , ( "o", Encode.list Program.encodeSingle ops )
                , ( "e", encodeEnvironment env )
                ]


valueDecoder : Decoder Value
valueDecoder =
    Decode.field "t" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "n" ->
                        Decode.map Integer <| Decode.field "v" Decode.int

                    "t" ->
                        Decode.succeed Truthy

                    "a" ->
                        Decode.map Array (Decode.field "v" <| Cons.decoder valueDecoder)

                    "c" ->
                        Decode.map2 Closure (Decode.field "o" <| Decode.list Program.singleDecoder) (Decode.field "e" environmentDecoder)

                    _ ->
                        Decode.fail <| "Decoding value: unknown tag " ++ tag
            )
