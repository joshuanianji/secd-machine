module SECD.VM exposing (..)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Lib.Colours as Colours
import Lib.Cons as Cons exposing (Cons)
import Lib.Util as Util
import List.Zipper as Zipper exposing (Zipper)
import SECD.EnvItem as EnvItem exposing (EnvItem)
import SECD.Error as Error exposing (Error)
import SECD.Program as Program exposing (Cmp, Func(..), Op(..), Program)



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
    List (EnvItem Value)


type alias Control =
    List Op


type alias Dump =
    List DumpValue



-- types of values that can be in the stack


type Value
    = Integer Int
    | Truthy -- false is represented by Nil
    | Array (Cons Value)
      -- closure holds the name of the function as well (if necessary)
    | Closure (Maybe String) (List Op) Environment -- When loading a function, dump the closure of the function (i think?)


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

        Closure _ _ _ ->
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

        Closure (Just funcName) _ _ ->
            "Closure of " ++ funcName

        Closure Nothing _ _ ->
            "Closure"



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
    VM initCtx [] [] (Program.toList prog) []



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
            Unfinished (VM ctx s (EnvItem.Dummy :: e) c_ d)

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

        coordsString =
            "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"
    in
    case locateInEnv ( x, y ) e ctx of
        Err errStr ->
            Error vm <| "VM: loadFromEnv: Variable not found from coords " ++ coordsString ++ "\n" ++ errStr

        Ok (Cons.Val a) ->
            Unfinished (VM ctx (a :: s) e c d)

        Ok consArr ->
            Unfinished (VM ctx (Array consArr :: s) e c d)



-- recursively looks through the environment


locateInEnv : ( Int, Int ) -> Environment -> Context -> Result String (Cons Value)
locateInEnv ( x, y ) env ctx =
    let
        locateRow : Int -> List a -> Result String a
        locateRow y_ row =
            case ( y_, row ) of
                ( _, [] ) ->
                    Err "y-value out of bounds!"

                ( 0, h :: _ ) ->
                    Ok h

                ( _, _ :: t ) ->
                    locateRow (y_ - 1) t
    in
    if (x < 0) || (y < 0) then
        Err "Negative index out of bounds"

    else
        case ( x, env ) of
            ( _, [] ) ->
                Err "Locate: out of bounds!"

            ( 0, (EnvItem.ListItem h) :: _ ) ->
                locateRow y h

            ( 0, EnvItem.Dummy :: _ ) ->
                case ctx.dummyVal of
                    Nothing ->
                        Err "Locate: attempt to access uninitialized Dummy Env value!"

                    Just dummyVal ->
                        locateRow y dummyVal

            ( _, _ :: t ) ->
                locateInEnv ( x - 1, y ) t ctx


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
        -- named function
        (FUNCBODY funcName funcBody) :: c_ ->
            Unfinished (VM ctx (Closure (Just funcName) funcBody e :: s) e c_ d)

        -- unnamed function (e.g. lambda)
        (NESTED funcbody) :: c_ ->
            Unfinished (VM ctx (Closure Nothing funcbody e :: s) e c_ d)

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
        (Closure _ funcBody env) :: (Array vals) :: s_ ->
            case Cons.toList vals of
                Nothing ->
                    Error vm "VM: applyFunction: badly formatted closure!"

                Just values ->
                    Unfinished (VM ctx [] (EnvItem.ListItem values :: env) funcBody (EntireState s_ e c :: d))

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
        ( (Closure f funcBody env) :: (Array vals) :: s_, EnvItem.Dummy :: e_ ) ->
            case ( Cons.toList vals, env ) of
                ( Nothing, _ ) ->
                    Error vm ("VM: recursiveApply: badly formatted closure" ++ Maybe.withDefault "!" (Maybe.map ((++) " from function ") f))

                ( _, [] ) ->
                    Error vm "VM: recursiveApply: empty environment!"

                ( Just values, EnvItem.Dummy :: env_ ) ->
                    -- (ListItem values :: env_) is the new environment
                    -- replace dummy value with the closure (?) of the recursive function i think
                    Unfinished (VM { ctx | dummyVal = Just values } [] (EnvItem.ListItem values :: env_) funcBody (EntireState s_ e_ c :: d))

                ( _, _ ) ->
                    Error vm "VM: recursiveApply: malformed environment!"

        _ ->
            Error vm "VM: recursiveApply: Cannot find function body!"



-- EVALUATION


type alias VMResult =
    Result Error Value


{-|


## evaluate

fully evaluates a VM, counting the amount of steps

_this is mainly used for testing_

**WARNING**: this will blow up the stack if it goes in an infinite loop

-}
evaluate : VM -> ( Int, VMResult )
evaluate inputVM =
    let
        evaluate_ : VM -> Int -> ( Int, VMResult )
        evaluate_ vm_ n =
            case step vm_ of
                Unfinished vm ->
                    evaluate_ vm (n + 1)

                Finished _ val ->
                    ( n, Ok val )

                Error _ err ->
                    ( n, Err err )
    in
    evaluate_ inputVM 0


{-|


## stepNAccumulate

Given a starting VM, step a max of "n" times, storing each VM state in a list.

`evalN n startVM` does not include startVM in the states we return.

  - If we're not finished, return `(vms, Err nextVM)`
  - If we're finished, return `(vms, Ok (vmResult, count))`

`nextVM` is not included in the accumulated states.

The VM returned if the program is not finished executing is also not included in the list of VM states

-}
stepNAccumulate : Int -> VM -> ( List VM, Result VM VMResult )
stepNAccumulate chunkSize vm =
    let
        helper : Int -> VM -> List VM -> ( List VM, Result VM VMResult )
        helper n vm_ states =
            case step vm_ of
                Unfinished newVm ->
                    if n == 0 then
                        ( states, Err newVm )

                    else
                        helper (n - 1) newVm (newVm :: states)

                Finished _ val ->
                    ( states, Ok <| Ok val )

                Error _ err ->
                    ( states, Ok <| Err err )
    in
    helper chunkSize vm []
        |> Tuple.mapFirst List.reverse


{-|


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
evalChunk : VM -> Int -> ( Int, Result VM VMResult )
evalChunk startVM chunkSize =
    let
        ( vms, result ) =
            stepNAccumulate (chunkSize - 1) startVM
    in
    ( List.length vms + 1, result )


{-|


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
evalPage : Int -> Int -> VM -> { totalVMCount : Int, chunkVMs : List VM, result : Result VM VMResult }
evalPage pageSize chunkSize vm =
    let
        -- evalPage helper that accumulates the chunkVMs in the parameter
        helper : VM -> Int -> Int -> List VM -> ( Int, List VM, Result VM VMResult )
        helper startVM vmCount chunkCount stateAcc =
            if chunkCount == pageSize then
                ( vmCount, stateAcc, Err startVM )

            else
                -- chunkVM is the first VM state of the chunk
                case evalChunk startVM chunkSize of
                    ( currChunkSize, Ok val ) ->
                        -- evaluation finished early. currChunkSize is less than chunkSize
                        ( vmCount + currChunkSize, startVM :: stateAcc, Ok val )

                    ( _, Err nextVM ) ->
                        helper nextVM (vmCount + chunkSize) (chunkCount + 1) (startVM :: stateAcc)
    in
    case evalChunk vm chunkSize of
        ( currChunkSize, Ok val ) ->
            -- evaluation finished in one chunk!
            { totalVMCount = currChunkSize, chunkVMs = [], result = Ok val }

        ( _, Err nextVM ) ->
            -- start from the NEXT chunk, since we don't want to include the input startChunk
            let
                ( totalVMCount, chunkVMs, result ) =
                    helper nextVM chunkSize 1 []
            in
            { totalVMCount = totalVMCount, chunkVMs = List.reverse chunkVMs, result = result }


{-| getPages

given a VM, calculate as many states it can reach
Limit the amount of pages (groups of "chunks" that are stored in SessionStorage similar to a memory page) by maxPages
Limit the amount of chunkVMs a page can store by pageSize
Limit the amount of stets a chunk can store by chunkSize

If we the VM terminates in time, also return the value
Else, return the last unfinished VM state

This function is used in VMView to initialize the model.

-}
type alias PagesData =
    { result : Result VM VMResult
    , initialPage : Zipper VM
    , initialChunk : Zipper VM
    , pages : Zipper Int
    , totalVMCount : Int
    , toJSValues : List ( Int, Encode.Value )
    }


getPages :
    { maxPages : Int
    , pageSize : Int
    , chunkSize : Int
    }
    -> VM
    -> PagesData
getPages { maxPages, pageSize, chunkSize } vm =
    let
        -- returns the same result as getPages
        getFirstChunk : PagesData
        getFirstChunk =
            let
                ( initialChunk, firstChunkRes ) =
                    stepNAccumulate (chunkSize - 1) vm
            in
            { result = firstChunkRes
            , initialPage = Zipper.singleton vm
            , initialChunk = Zipper.fromCons vm initialChunk
            , pages = Zipper.singleton 0

            -- adding one because the initial `vm` is not accounted for in the list of VMs
            , totalVMCount = List.length initialChunk + 1
            , toJSValues = []
            }

        -- takes in, and returns the same result as getPages
        getFirstPage : PagesData -> PagesData
        getFirstPage pagesData =
            case pagesData.result of
                Ok _ ->
                    pagesData

                -- we don't care about the nextState, since we will recalculate the page starting from the initial VM
                Err _ ->
                    -- program needs more than one chunk
                    -- calculate first page
                    let
                        firstPage =
                            evalPage pageSize chunkSize vm
                    in
                    { pagesData
                        | result = firstPage.result
                        , initialPage = Zipper.fromCons vm firstPage.chunkVMs
                        , totalVMCount = firstPage.totalVMCount
                    }

        getRemainingPages : PagesData -> PagesData
        getRemainingPages pagesData =
            case pagesData.result of
                Ok _ ->
                    pagesData

                Err secondPageVM ->
                    -- VM does not terminate in one page. Calculate all pages (until max)
                    let
                        -- we also send the first page states to JS
                        helperData =
                            remainingPagesHelper 1 secondPageVM 0 [ ( 0, Encode.list encode <| Zipper.toList pagesData.initialPage ) ]
                    in
                    { pagesData
                        | result = helperData.result
                        , pages = Zipper.fromCons 0 (List.range 1 helperData.pageCount)
                        , totalVMCount = pagesData.totalVMCount + helperData.totalVMCount
                        , toJSValues = helperData.portData
                    }

        -- builds up all the pages to send to JS
        remainingPagesHelper :
            Int
            -> VM
            -> Int
            -> List ( Int, Encode.Value )
            ->
                { result : Result VM VMResult
                , pageCount : Int
                , totalVMCount : Int
                , portData : List ( Int, Encode.Value )
                }
        remainingPagesHelper pageCount vm_ vmCountAcc portDataAcc =
            if pageCount == maxPages then
                { result = Err vm_
                , pageCount = pageCount
                , totalVMCount = vmCountAcc
                , portData = portDataAcc
                }

            else
                let
                    nthPage =
                        evalPage pageSize chunkSize vm_
                in
                case nthPage.result of
                    Ok vmResult ->
                        -- VM terminates in this page
                        { result = Ok vmResult
                        , pageCount = pageCount
                        , totalVMCount = vmCountAcc + nthPage.totalVMCount
                        , portData = ( pageCount, Encode.list encode (vm_ :: nthPage.chunkVMs) ) :: portDataAcc
                        }

                    Err newVM ->
                        remainingPagesHelper (pageCount + 1)
                            newVM
                            (vmCountAcc + nthPage.totalVMCount)
                            (( pageCount, Encode.list encode (vm_ :: nthPage.chunkVMs) ) :: portDataAcc)
    in
    getFirstChunk
        |> getFirstPage
        |> getRemainingPages



-- VIEW


view : Int -> VM -> Element msg
view n (VM ctx s e c d) =
    Element.column
        [ Element.width Element.fill
        , Font.size 18
        ]
        [ Element.wrappedRow
            [ Element.centerX
            , Element.paddingXY 12 18
            , Element.spacing 6
            ]
            [ viewStack n s
            , viewEnv n e
            , viewControl n c
            , viewDump n d
            ]
        , Element.el [ Element.centerX ] <| viewContext n ctx
        ]


viewContext : Int -> Context -> Element msg
viewContext n ctx =
    let
        dummyVal =
            case ctx.dummyVal of
                Nothing ->
                    Element.text "Nothing"

                Just v ->
                    List.map (Cons.view (viewValue n)) v
                        |> List.intersperse (Element.text ",")
                        |> Element.row []
    in
    Element.row []
        [ Element.text "dummyVal = ", dummyVal ]



-- generic stack view


viewVMStack : { n : Int, viewStackChunk : List a -> List (Element msg), stack : List a, stackName : String } -> Element msg
viewVMStack { n, viewStackChunk, stack, stackName } =
    let
        take =
            if n == 0 then
                identity

            else
                List.take n

        -- instead of having an opinionated view of the entire stack, we have a "view stack chunk" input
        -- this allows the stack view function to dictate how to view a list of elements
        -- for example, the control stack wants to flatten nested code, so the code wraps nicely.
        -- this would not be possible with a "viewStackElem" function
        stackElements =
            viewStackChunk (take stack)
                |> Util.addIf (List.length stack > 0) [ Element.text "." ]
                |> Util.addIf True [ trailingS ]

        trailingS =
            viewStackName stackName (List.drop n stack)
    in
    Element.row
        ([ Element.spacing 2
         , Element.paddingXY 8 4
         ]
            |> addHoverable
        )
        stackElements



-- view a VM value


viewValue : Int -> Value -> Element msg
viewValue n val =
    case val of
        Integer i ->
            Element.text (String.fromInt i)

        Truthy ->
            Element.text "true"

        Array arr ->
            Cons.view (viewValue n) arr

        Closure mName f env ->
            let
                stackName =
                    case env of
                        EnvItem.Dummy :: env_ ->
                            Element.row
                                [ Element.spacing 2 ]
                                [ Element.text "("
                                , EnvItem.view (viewValue n) EnvItem.Dummy
                                , Element.text "."
                                , viewStackName "e" env_
                                , Element.text ")"
                                ]

                        _ ->
                            viewStackName "e" env
            in
            Element.row
                ([ Element.spacing 2
                 , Element.width Element.fill
                 , Element.paddingXY 8 4
                 ]
                    |> addHoverable
                )
                [ Element.text "("

                -- Show name if possible, else say "BODY", expandable upon click
                , Maybe.withDefault (Element.text "BODY") (Maybe.map Element.text mName)

                -- show just "e", more data on click
                -- , viewEnv n env
                , Element.text "."
                , stackName
                , Element.text ")"
                ]


viewStack : Int -> Stack -> Element msg
viewStack n s =
    viewVMStack
        { n = n
        , viewStackChunk =
            List.map (viewValue n)
                >> List.intersperse (Element.text " ")
        , stack = s
        , stackName = "s"
        }


viewEnv : Int -> Environment -> Element msg
viewEnv n e =
    viewVMStack
        { n = n
        , viewStackChunk =
            List.map (EnvItem.view <| viewValue n)
                >> List.intersperse (Element.text " ")
        , stack = e
        , stackName = "e"
        }


viewControl : Int -> Control -> Element msg
viewControl n c =
    viewVMStack
        { n = n

        -- we want to flatten the control stack, so nested code can wrap nicely
        , viewStackChunk =
            List.map Program.view
                >> List.intersperse [ Element.text " " ]
                >> List.concat
        , stack = c
        , stackName = "c"
        }


viewDump : Int -> Dump -> Element msg
viewDump n d =
    viewVMStack
        { n = n
        , viewStackChunk =
            List.map (viewDumpVal n)
                >> List.intersperse (Element.text " ")
        , stack = d
        , stackName = "d"
        }


viewDumpVal : Int -> DumpValue -> Element msg
viewDumpVal n dv =
    case dv of
        Control c ->
            viewStackName "c" c

        EntireState s e c ->
            Element.row
                ([ Element.paddingXY 6 4
                 , Element.spacing 8
                 ]
                    |> addHoverable
                )
                [ viewStackName "s" s
                , viewStackName "e" e
                , viewStackName "c" c
                ]



-- view helpers
-- view a stack name (e.g. "e") based on its length
-- if the length of zero, make it light


viewStackName : String -> List a -> Element msg
viewStackName name stack =
    if List.length stack > 0 then
        Element.el [] (Element.text name)

    else
        Element.el [ Font.color Colours.lightGrey ] (Element.text name)


addHoverable : List (Attribute msg) -> List (Attribute msg)
addHoverable attrs =
    attrs
        ++ [ Border.rounded 4
           , Element.mouseOver
                [ Background.color Colours.slateGrey ]
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
    Encode.list (EnvItem.encode encodeValue)


environmentDecoder : Decoder Environment
environmentDecoder =
    Decode.list (EnvItem.decoder valueDecoder)



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

        Closure mName ops env ->
            Encode.object
                [ ( "t", Encode.string "c" )
                , ( "n", Maybe.withDefault Encode.null <| Maybe.map Encode.string mName )
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
                        Decode.map3 Closure (Decode.field "n" <| Decode.nullable Decode.string) (Decode.field "o" <| Decode.list Program.singleDecoder) (Decode.field "e" environmentDecoder)

                    _ ->
                        Decode.fail <| "Decoding value: unknown tag " ++ tag
            )
