module SECD.VM exposing (..)

import Lib.Cons as Cons exposing (Cons)
import SECD.Environment as Env exposing (Environment)
import SECD.Error exposing (Error)
import SECD.Program as Program exposing (Cmp, Func(..), Op(..), Program)



-- | SECD Virtual Machine
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
    | Boolean Bool
    | Array (Cons Value)



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


vmAtom : Value -> Result String Value
vmAtom v =
    case v of
        Boolean _ ->
            Ok <| Boolean True

        Integer _ ->
            Ok <| Boolean True

        -- nil is the only thing that is both an atom and a list
        Array Cons.Nil ->
            Ok <| Boolean True

        Array _ ->
            Ok <| Boolean False



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


vmCompare : Cmp -> Value -> Value -> Result String Value
vmCompare cmp va vb =
    case ( va, vb ) of
        ( Integer a, Integer b ) ->
            Ok <| Boolean <| Program.cmpFunc cmp a b

        _ ->
            Err <| "compare: Expecting two integers, got " ++ valueToString va ++ " and " ++ valueToString vb


valueToString : Value -> String
valueToString val =
    case val of
        Integer i ->
            String.fromInt i

        Boolean True ->
            "true"

        Boolean False ->
            "false"

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
    case ( s, e, c ) of
        ( _, _, [] ) ->
            endProgram s vm

        -- evaluate Nil
        ( _, _, NIL :: c_ ) ->
            Unfinished (VM (nil :: s) e c_ d)

        -- evaluate Load Constant
        ( _, _, (LDC x) :: c_ ) ->
            Unfinished (VM (Integer x :: s) e c_ d)

        -- evaluate load variable
        ( _, _, (LD ( x, y )) :: c_ ) ->
            loadFromEnv ( x, y ) (VM s e c_ d)

        -- Binary or unary operator
        ( _, _, (FUNC f) :: c_ ) ->
            evalFunc f (VM s e c_ d)

        -- IF/THEN/ELSE
        ( _, _, SEL :: c_ ) ->
            evalIfElse (VM s e c_ d)

        ( _, _, JOIN :: c_ ) ->
            endIfElse (VM s e c_ d)

        -- Load and evaluating functions
        ( _, _, LDF :: c_ ) ->
            loadFunction (VM s e c_ d)

        ( _, _, AP :: c_ ) ->
            applyFunction (VM s e c_ d)

        ( _, _, RTN :: c_ ) ->
            returnFromFunction (VM s e c_ d)

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

        ATOM ->
            evalUnary vmAtom vm

        CONS ->
            evalBinary vmCons vm

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

        ( (Boolean b) :: s_, (NESTED ct) :: (NESTED cf) :: c_ ) ->
            let
                newC =
                    if b then
                        ct

                    else
                        cf

                -- update dump value to hold the control stack after the if/else
                newD =
                    Control c_ :: d
            in
            Unfinished (VM s_ e newC newD)

        ( (Boolean _) :: _, _ ) ->
            Error vm "VM: evalIfElse: Failure to complete if/else: Control stack is not Nested"

        ( a :: _, _ ) ->
            Error vm <| "VM: evalIfElse: Expected boolean value, got " ++ valueToString a


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
        ( (Array arr) :: s_, (Closure funcBody env) :: d_ ) ->
            case Cons.toList arr of
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


evaluate : VM -> Result Error Value
evaluate oldVm =
    case step oldVm of
        Unfinished vm ->
            evaluate vm

        Finished _ val ->
            Ok val

        Error _ err ->
            Err err
