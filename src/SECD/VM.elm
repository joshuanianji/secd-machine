module SECD.VM exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Lib.Cons as Cons exposing (Cons)
import SECD.Error as Error exposing (Error)
import SECD.Program as Program exposing (Cmp, Func(..), Op(..), Program)
import SECD.VMEnv as Env exposing (EnvItem(..), Environment)



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
        (Environment Value)
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


type alias Control =
    List Op


type alias Dump =
    List DumpValue



-- types of values that can be in the stack


type Value
    = Integer Int
    | Truthy -- false is represented by Nil
    | Array (Cons Value)
    | Closure (List Op) (Environment Value) -- When loading a function, dump the closure of the function (i think?)


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
    | EntireState Stack (Environment Value) Control -- When applying a function, we want to dump the state of the VM



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



-- fully evaluate a VM
-- WARNING: this will blow up the stack if it goes in an infinite loop
-- this is mainly used for testing


evaluate : VM -> Result Error Value
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



-- evaluate a VM a max of n times
-- If we're not finished, return Err (currState, allStates)
-- If we're finished, return Ok (returnValue, allStates)


evalN : VM -> Int -> Result ( VM, List State ) ( Result Error Value, List State )
evalN vm n =
    let
        helper : Int -> VM -> List State -> Result ( VM, List State ) ( Result Error Value, List State )
        helper n_ vm_ states =
            if n_ == 0 then
                Err ( vm, List.reverse states )

            else
                case step vm_ of
                    Unfinished newVm ->
                        helper (n_ - 1) newVm (Unfinished vm :: states)

                    Finished _ val ->
                        Ok ( Ok val, states )

                    Error _ err ->
                        Ok ( Err err, states )
    in
    helper n vm []



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
        [ ( "dummyVal", encodedDummyVal ) ]


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
        (Decode.field "dummyVal" dummyValDecoder)



-- STACK
-- just a list of values


encodeStack : Stack -> Encode.Value
encodeStack =
    Encode.list encodeValue


stackDecoder : Decoder Stack
stackDecoder =
    Decode.list valueDecoder



-- ENVIRONMENT


encodeEnvironment : Environment Value -> Encode.Value
encodeEnvironment =
    Env.encode encodeValue


environmentDecoder : Decoder (Environment Value)
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
                        [ ( "tag", Encode.string "Control" )
                        , ( "val", encodeControl c )
                        ]

                EntireState s e c ->
                    Encode.object
                        [ ( "tag", Encode.string "EntireState" )
                        , ( "stack", encodeStack s )
                        , ( "environment", encodeEnvironment e )
                        , ( "control", encodeControl c )
                        ]
    in
    Encode.list encodeDumpVal


dumpDecoder : Decoder Dump
dumpDecoder =
    let
        dumpValueDecoder =
            Decode.field "tag" Decode.string
                |> Decode.andThen
                    (\tag ->
                        case tag of
                            "Control" ->
                                Decode.map Control <| Decode.field "val" controlDecoder

                            "EntireState" ->
                                Decode.map3 EntireState
                                    (Decode.field "stack" stackDecoder)
                                    (Decode.field "environment" environmentDecoder)
                                    (Decode.field "control" controlDecoder)

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
                [ ( "tag", Encode.string "Int" )
                , ( "val", Encode.int n )
                ]

        Truthy ->
            Encode.object
                [ ( "tag", Encode.string "Truthy" ) ]

        Array cons ->
            Encode.object
                [ ( "tag", Encode.string "Array" )
                , ( "val", Cons.encode encodeValue cons )
                ]

        Closure ops env ->
            Encode.object
                [ ( "tag", Encode.string "Closure" )
                , ( "ops", Encode.list Program.encodeSingle ops )
                , ( "env", encodeEnvironment env )
                ]


valueDecoder : Decoder Value
valueDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Int" ->
                        Decode.map Integer <| Decode.field "val" Decode.int

                    "Truthy" ->
                        Decode.succeed Truthy

                    "Array" ->
                        Decode.map Array <| Cons.decoder valueDecoder

                    "Closure" ->
                        Decode.map2 Closure (Decode.list Program.singleDecoder) environmentDecoder

                    _ ->
                        Decode.fail <| "Decoding value: unknown tag " ++ tag
            )
