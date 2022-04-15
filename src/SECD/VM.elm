module SECD.VM exposing (..)

import SECD.Error exposing (Error)
import SECD.Program as Program exposing (Op(..), Program)



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
step (VM s e c d) =
    case ( s, e, c ) of
        ( _, _, [] ) ->
            finishEval s (VM s e c d)

        -- evaluate Nil
        ( _, _, NIL :: c_ ) ->
            Unfinished (VM (Nil :: s) e c_ d)

        -- evaluate Load Constant
        ( _, _, (LDC x) :: c_ ) ->
            Unfinished (VM (Integer x :: s) e c_ d)

        _ ->
            Error (VM s e c d) "VM: step: VM is in an invalid state"



-- finished an evaluation when the Control stack is empty


finishEval : Stack -> VM -> State
finishEval stack vm =
    case stack of
        [] ->
            Error vm "Stack is empty on program exit"

        val :: _ ->
            Finished vm val


evaluate : VM -> Result Error Value
evaluate oldVm =
    case step oldVm of
        Unfinished vm ->
            evaluate vm

        Finished _ val ->
            Ok val

        Error _ err ->
            Err err
