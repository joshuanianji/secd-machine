module SECD.VM exposing (..)

import SECD.Error exposing (Error)
import SECD.Program as Program exposing (Func(..), Op(..), Program)



-- | SECD Virtual Machine
---- Types ----


type VM
    = VM
        Stack
        -- Stack used for evaluation of expressions
        Environment
        --  Environment used to store the current value list
        Control
        -- Control used to store the current instruction pointer
        Dump



-- Dump stack used to store the current state of the VM


type alias Stack =
    List Value



-- holds the context during closures
-- i should optimize it


type alias Environment =
    List (List Value)


type alias Control =
    List Op


type alias Dump =
    List DumpValue



-- types of values we can return from the VM
-- TODO: add list


type Value
    = Integer Int
    | Boolean Bool
    | Nil



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
        Nil ->
            Ok <| Boolean False

        Boolean _ ->
            Ok <| Boolean True

        Integer _ ->
            Ok <| Boolean True


valueToString : Value -> String
valueToString val =
    case val of
        Integer i ->
            String.fromInt i

        Boolean True ->
            "true"

        Boolean False ->
            "false"

        Nil ->
            "Nil"



-- types of values we can store in the dump stack


type DumpValue
    = Control (List Op) -- in Sel, we want to dump the contents of the control stack
    | EntireState Stack Environment Control -- When applying a function, we want to dump the state of the VM



---- VM Operations ----


init : Program -> VM
init prog =
    VM [] [] (Program.toList prog) []



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
            finishEval s vm

        -- evaluate Nil
        ( _, _, NIL :: c_ ) ->
            Unfinished (VM (Nil :: s) e c_ d)

        -- evaluate Load Constant
        ( _, _, (LDC x) :: c_ ) ->
            Unfinished (VM (Integer x :: s) e c_ d)

        -- Binary or unary operator
        ( _, _, (Func f) :: c_ ) ->
            evalFunc f (VM s e c_ d)

        -- IF/THEN/ELSE
        ( _, _, SEL :: c_ ) ->
            evalIfElse (VM s e c_ d)

        ( _, _, JOIN :: c_ ) ->
            endIfElse (VM s e c_ d)

        _ ->
            Error vm "VM: step: VM is in an invalid state"



-- finished an evaluation when the Control stack is empty


finishEval : Stack -> VM -> State
finishEval stack vm =
    case stack of
        [] ->
            Error vm "Stack is empty on program exit"

        val :: _ ->
            Finished vm val


evalFunc : Func -> VM -> State
evalFunc f vm =
    case f of
        Add ->
            evalBinary vmAdd vm

        Mult ->
            evalBinary vmMultiply vm

        Atom ->
            evalUnary vmAtom vm


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

        ( (Boolean b) :: s_, (Nested ct) :: (Nested cf) :: c_ ) ->
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


evaluate : VM -> Result Error Value
evaluate oldVm =
    case step oldVm of
        Unfinished vm ->
            evaluate vm

        Finished _ val ->
            Ok val

        Error _ err ->
            Err err
