module Views.Compiled exposing
    ( APType(..)
    , Code(..)
    , Indexed(..)
    , Model
    , Msg
    , Unindexed(..)
    , getIndices
    , init
    , stripIndices
    , subscriptions
    , transpile
    , update
    , view
    )

-- | Compiled View
-- Views the compiled code.

import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Lib.Colours as Colours
import Lib.Util as Util
import Lib.Views
import List.Zipper as Zipper exposing (Zipper)
import Ordinal exposing (ordinal)
import Ports
import SECD.Error as Error exposing (Error)
import SECD.Program as Prog exposing (Program)
import SECD.VM as VM exposing (VM, VMResult)



---- MODEL ----


type alias Model =
    { -- remove original after more testing
      original : List Prog.Op
    , stuff : Result Error OkModel
    }


type alias OkModel =
    { code : List Indexed }


init : Program -> ( Model, Cmd Msg )
init prog =
    ( { original = Prog.toList prog
      , stuff = Result.map (\code -> { code = code }) (transpile <| Prog.toList prog)
      }
    , Cmd.none
    )



-- convert the Program code to our internal representation, useful for UI stuff


transpile : List Prog.Op -> Result Error (List Indexed)
transpile =
    transpile_ 0 >> Result.map Tuple.second



-- transpile helper function alsp keeps track of a "counter" to uniquely identify each code node


transpile_ : Int -> List Prog.Op -> Result Error ( Int, List Indexed )
transpile_ n ops =
    case ops of
        Prog.NIL :: xs ->
            initSingle n NIL
                |> prependTranspiled xs

        (Prog.LD coords) :: xs ->
            initSingle n (LD coords)
                |> prependTranspiled xs

        (Prog.LDC x) :: xs ->
            initSingle n (LDC x)
                |> prependTranspiled xs

        Prog.LDF :: (Prog.FUNCBODY name _) :: xs ->
            initSingle n (LDFunc name)
                |> prependTranspiled xs

        Prog.LDF :: (Prog.NESTED nested) :: Prog.AP :: xs ->
            initChain n (\funcBody rest -> Indexed ( n, LDApply AP funcBody ) :: rest)
                |> chainTranspiled nested
                |> chainTranspiled xs

        Prog.LDF :: (Prog.NESTED nested) :: Prog.RAP :: xs ->
            initChain n (\funcBody rest -> Indexed ( n, LDApply RAP funcBody ) :: rest)
                |> chainTranspiled nested
                |> chainTranspiled xs

        Prog.LDF :: (Prog.NESTED nested) :: xs ->
            initChain n (\funcBody rest -> Indexed ( n, LDLambda funcBody ) :: rest)
                |> chainTranspiled nested
                |> chainTranspiled xs

        Prog.RTN :: xs ->
            initSingle n RTN
                |> prependTranspiled xs

        Prog.SEL :: (Prog.NESTED nestedT) :: (Prog.NESTED nestedF) :: xs ->
            initChain n (\onT onF rest -> Indexed ( n, SEL onT onF ) :: rest)
                |> chainTranspiled nestedT
                |> chainTranspiled nestedF
                |> chainTranspiled xs

        Prog.JOIN :: xs ->
            initSingle n JOIN
                |> prependTranspiled xs

        Prog.DUM :: xs ->
            initSingle n DUM
                |> prependTranspiled xs

        (Prog.FUNC f) :: xs ->
            initSingle n (FUNC <| Prog.funcToString f)
                |> prependTranspiled xs

        Prog.AP :: xs ->
            initSingle n LoneAP
                |> prependTranspiled xs

        [] ->
            Ok ( n, [] )

        op :: _ ->
            Err <| "Unexpected op! " ++ Prog.opToString op



-- when we just want one element
-- Kind of like a monadic succeed


initChain : Int -> a -> Result Error ( Int, a )
initChain n a =
    Ok ( n + 1, a )



-- special case of initChain


initSingle : Int -> Code Indexed -> Result Error ( Int, Indexed )
initSingle n a =
    Ok ( n + 1, Indexed ( n, a ) )



-- add an element to the chain
-- a bit like a monadic bind


chainTranspiled : List Prog.Op -> Result Error ( Int, List Indexed -> b ) -> Result Error ( Int, b )
chainTranspiled ops =
    Result.andThen
        (\( startN, f ) ->
            transpile_ startN ops
                |> Result.map (Tuple.mapSecond <| \rest -> f rest)
        )



-- add a transpiled list to the end of a single indexed code
-- this is a special case of chain


prependTranspiled : List Prog.Op -> Result Error ( Int, Indexed ) -> Result Error ( Int, List Indexed )
prependTranspiled ops =
    Result.andThen
        (\( n, idxed ) ->
            transpile_ (n + 1) ops
                |> Result.map (Tuple.mapSecond <| \rest -> idxed :: rest)
        )



-- each code block will be indexed by a unique ID


type Indexed
    = Indexed ( Int, Code Indexed )



-- an unindexed code block is just the code itself. Stripping the indices is sometimes useful for testing


type Unindexed
    = Unindexed (Code Unindexed)


stripIndices : Indexed -> Unindexed
stripIndices (Indexed ( _, code )) =
    case code of
        NIL ->
            Unindexed NIL

        LD coords ->
            Unindexed (LD coords)

        LDC x ->
            Unindexed (LDC x)

        LDFunc name ->
            Unindexed (LDFunc name)

        LDApply aptype nested ->
            Unindexed (LDApply aptype (List.map stripIndices nested))

        LDLambda nested ->
            Unindexed (LDLambda (List.map stripIndices nested))

        SEL nestedT nestedF ->
            Unindexed (SEL (List.map stripIndices nestedT) (List.map stripIndices nestedF))

        RTN ->
            Unindexed RTN

        JOIN ->
            Unindexed JOIN

        DUM ->
            Unindexed DUM

        FUNC f ->
            Unindexed (FUNC f)

        LoneAP ->
            Unindexed LoneAP



-- other times, we only want the indices


getIndices : List Indexed -> List Int
getIndices ops =
    let
        getIndex : Indexed -> List Int
        getIndex (Indexed ( n, code )) =
            case code of
                LDApply _ nested ->
                    n :: getIndices nested

                LDLambda nested ->
                    n :: getIndices nested

                SEL nestedT nestedF ->
                    n :: getIndices nestedT ++ getIndices nestedF

                -- single elements
                _ ->
                    [ n ]
    in
    List.map getIndex ops
        |> List.concat



-- I need to beef up the program type (List Op) into something more helpful
-- Thus, I traverse the compiled ops and generate my own tree structure used just for a interactive with the code
-- a lot of these changes can simply be built into the Program type itself, but I want to keep the program type as simple as possible


type Code a
    = NIL
    | LD ( Int, Int )
    | LDC Int
    | LDFunc String -- Loads a function name (when a function is defined in a let stmt)
    | LDLambda (List a) -- loads a lambda (when a lambda is an argument to a function)
    | LDApply APType (List a) -- loads a function and apply it
    | LoneAP -- if a function is loaded from the env then applied
    | RTN
    | SEL (List a) (List a)
    | JOIN
    | DUM
    | FUNC String -- Builtin function (just use their string representation lol)


type APType
    = AP
    | RAP



---- MSG ----


type Msg
    = NoOp



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    case model.stuff of
        Err e ->
            Element.html <| Error.view e

        Ok okModel ->
            viewOk okModel


viewOk : OkModel -> Element Msg
viewOk model =
    Debug.toString model.code
        |> Element.text
        |> List.singleton
        |> Element.paragraph [ Font.size 20 ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
