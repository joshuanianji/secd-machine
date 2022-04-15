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


type Value
    = Integer Int
    | Boolean Bool
    | Nil


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
            evalFunc f s (VM s e c_ d)

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


evalFunc : Func -> Stack -> VM -> State
evalFunc f stack vm =
    case f of
        Add ->
            evalBinary (+) stack vm

        Mult ->
            evalBinary (*) stack vm


evalBinary : (Int -> Int -> Int) -> Stack -> VM -> State
evalBinary op stack vm =
    let
        (VM s e c d) =
            vm
    in
    case stack of
        (Integer x) :: (Integer y) :: s_ ->
            Unfinished (VM (Integer (op y x) :: s_) e c d)

        a :: b :: _ ->
            Error vm <| "VM: evalBinary: Expected two integers to add, got " ++ valueToString a ++ " and " ++ valueToString b

        _ ->
            Error vm <|
                "VM: evalBinary: Stack of size "
                    ++ (String.fromInt <| List.length s)
                    ++ " does not have two elements!"


evaluate : VM -> Result Error Value
evaluate oldVm =
    case step oldVm of
        Unfinished vm ->
            evaluate vm

        Finished _ val ->
            Ok val

        Error _ err ->
            Err err
