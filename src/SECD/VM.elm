module SECD.VM exposing (..)

import Lib.Cons as Cons exposing (Cons)
import SECD.Environment as Env exposing (Environment)
import SECD.Error exposing (Error)
import SECD.Program as Program exposing (Cmp, Func(..), Op(..), Program)



-- | SECD Virtual Machine
-- | Runs an SECD program
---- Types ----


type VM
    = VM
        -- Stack used for evaluation of expressions
        Stack
        --  Environment used to store the current value list
        (Environment Value)
        -- Control used to store the current instruction pointer
        Control
        -- Dump to store anything else
        Dump



-- Dump stack used to store the current state of the VM


type alias Stack =
    List Value


type alias Control =
    List Op


type alias Dump =
    List DumpValue



-- types of values we can return from the VM


type Value
    = Integer Int
    | Truthy -- false is represented by Nil
    | Array (Cons Value)


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



-- joins a head and tail list


vmCons : Value -> Value -> Result String Value
vmCons head tail =
    case ( head, tail ) of
        ( Array ch, Array ct ) ->
            Ok <| Array (Cons.Cons ch ct)

        ( h, Array ct ) ->
            Ok <| Array (Cons.cons (Cons.single h) ct)

        _ ->
            Err <| "cons: Expecting an array and a value, got " ++ valueToString head ++ " and " ++ valueToString tail


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
            Cons.toString arr valueToString



-- types of values we can store in the dump stack


type DumpValue
    = Control (List Op) -- in Sel, we want to dump the contents of the control stack
    | EntireState Stack (Environment Value) Control -- When applying a function, we want to dump the state of the VM
    | Closure (List Op) (Environment Value) -- When loading a function, dump the closure of the function (i think?)



---- VM Operations ----


init : Program -> VM
init prog =
    VM [] Env.init (Program.toList prog) []



-- | Evaluate a program


type State
    = Unfinished VM
    | Finished VM Value
    | Error VM String


step : VM -> State
step vm =
    let
        (VM s e c d) =
            vm
    in
    case c of
        [] ->
            endProgram s vm

        -- evaluate Nil
        NIL :: c_ ->
            Unfinished (VM (nil :: s) e c_ d)

        -- evaluate Load Constant
        (LDC x) :: c_ ->
            Unfinished (VM (Integer x :: s) e c_ d)

        -- evaluate load variable
        (LD ( x, y )) :: c_ ->
            loadFromEnv ( x, y ) (VM s e c_ d)

        -- Binary or unary operator
        (FUNC f) :: c_ ->
            evalFunc f (VM s e c_ d)

        -- IF/THEN/ELSE
        SEL :: c_ ->
            evalIfElse (VM s e c_ d)

        JOIN :: c_ ->
            endIfElse (VM s e c_ d)

        -- Load and evaluating functions
        LDF :: c_ ->
            loadFunction (VM s e c_ d)

        AP :: c_ ->
            applyFunction (VM s e c_ d)

        RTN :: c_ ->
            returnFromFunction (VM s e c_ d)

        -- Recursive functions
        DUM :: c_ ->
            Unfinished (VM s (Env.pushDummy e) c_ d)

        RAP :: c_ ->
            recursiveApply (VM s e c_ d)

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
loadFromEnv ( x, y ) (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case Env.locate ( x, y ) e of
        Err errStr ->
            Error vm <| "VM: loadFromEnv: Variable not found from coords (" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")\n" ++ errStr

        Ok val ->
            Unfinished (VM (val :: s) e c d)


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
evalBinary op (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case s of
        a :: b :: s_ ->
            case op a b of
                Ok val ->
                    Unfinished (VM (val :: s_) e c d)

                Err msg ->
                    Error vm ("VM: evalBinary: " ++ msg)

        _ ->
            Error vm <|
                "VM: evalBinary: Stack of size "
                    ++ (String.fromInt <| List.length s)
                    ++ " does not have two elements!"


evalUnary : (Value -> Result String Value) -> VM -> State
evalUnary op (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case s of
        a :: s_ ->
            case op a of
                Ok val ->
                    Unfinished (VM (val :: s_) e c d)

                Err msg ->
                    Error vm ("VM: evalUnary: " ++ msg)

        _ ->
            Error vm <|
                "VM: evalUnary: Stack of size "
                    ++ (String.fromInt <| List.length s)
                    ++ " is empty!"


evalIfElse : VM -> State
evalIfElse (VM s e c d) =
    let
        vm =
            VM s e c d
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
            Unfinished (VM s_ e ct newD)

        ( (Array Cons.Nil) :: s_, (NESTED _) :: (NESTED cf) :: c_ ) ->
            let
                -- update dump value to hold the control stack after the if/else
                newD =
                    Control c_ :: d
            in
            Unfinished (VM s_ e cf newD)

        ( a :: _, _ ) ->
            case a of
                Truthy ->
                    Error vm "VM: evalIfElse: Failure to complete if/else: Control stack is not Nested (Can't find case branch)"

                Array Cons.Nil ->
                    Error vm "VM: evalIfElse: Failure to complete if/else: Control stack is not Nested (Can't find case branches)"

                _ ->
                    Error vm <| "VM: evalIfElse: Expected boolean-ish value, got " ++ valueToString a


endIfElse : VM -> State
endIfElse (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case d of
        (Control c_) :: d_ ->
            Unfinished (VM s e c_ d_)

        _ ->
            Error vm "VM: endIfElse: Dump stack does not hold control values"


loadFunction : VM -> State
loadFunction (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case c of
        (NESTED funcBody) :: c_ ->
            -- Note: in class, we dump the closure in the stack, but I think it's more consistent to move it to the dump.
            Unfinished (VM s e c_ (Closure funcBody e :: d))

        _ ->
            Error vm "VM: loadFunction: cannot find function body!"


applyFunction : VM -> State
applyFunction (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case ( s, d ) of
        -- vals is a list of the values of the arguments
        ( (Array vals) :: s_, (Closure funcBody env) :: d_ ) ->
            case Cons.toList vals of
                Nothing ->
                    Error vm "VM: applyFunction: badly formatted closure!"

                Just values ->
                    Unfinished (VM [] (Env.push values env) funcBody (EntireState s_ e c :: d_))

        _ ->
            Error vm "VM: applyFunction: cannot find function body!"


returnFromFunction : VM -> State
returnFromFunction (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case ( s, d ) of
        ( x :: _, (EntireState s_ e_ c_) :: d_ ) ->
            Unfinished (VM (x :: s_) e_ c_ d_)

        ( _, (EntireState _ _ _) :: _ ) ->
            Error vm "VM: returnFromFunction: Cannot find function return value!"

        _ ->
            Error vm "VM: returnFromFunction: Dump stack does not hold correct values!"


recursiveApply : VM -> State
recursiveApply (VM s e c d) =
    let
        vm =
            VM s e c d
    in
    case ( s, e, d ) of
        ( (Array vals) :: s_, Env.Dummy :: e_, (Closure funcBody env) :: d_ ) ->
            case Cons.toList vals of
                Nothing ->
                    Error vm "VM: applyFunction: badly formatted closure!"

                Just values ->
                    case Env.replaceDummy values env of
                        Ok newEnv ->
                            Unfinished (VM [] newEnv funcBody (EntireState s_ e_ c :: d_))

                        Err errStr ->
                            Error vm ("VM: recursiveApply: " ++ errStr)

        _ ->
            Error vm "VM: recursiveApply: Cannot find function body!"


evaluate : VM -> Result Error Value
evaluate oldVm =
    case step oldVm of
        Unfinished vm ->
            evaluate vm

        Finished _ val ->
            Ok val

        Error vm err ->
            Err <| err ++ Debug.toString vm
