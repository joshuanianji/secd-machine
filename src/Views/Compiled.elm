module Views.Compiled exposing
    ( APType(..)
    , Code(..)
    , Model
    , Msg
    , init
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
    { code : List Code }


init : Program -> ( Model, Cmd Msg )
init prog =
    ( { original = Prog.toList prog
      , stuff = Result.map (\code -> { code = code }) (transpile <| Prog.toList prog)
      }
    , Cmd.none
    )



-- convert the Program code to our internal representation, useful for UI stuff


transpile : List Prog.Op -> Result Error (List Code)
transpile ops =
    case ops of
        Prog.NIL :: xs ->
            Result.map ((::) NIL) <| transpile xs

        (Prog.LD coords) :: xs ->
            Result.map ((::) <| LD coords) <| transpile xs

        (Prog.LDC x) :: xs ->
            Result.map ((::) <| LDC x) <| transpile xs

        Prog.LDF :: (Prog.FUNCBODY name _) :: xs ->
            Result.map ((::) <| LDFunc name) <| transpile xs

        Prog.LDF :: (Prog.NESTED nested) :: Prog.AP :: xs ->
            Result.map2 (\nest rest -> LDApply nest AP :: rest) (transpile nested) (transpile xs)

        Prog.LDF :: (Prog.NESTED nested) :: Prog.RAP :: xs ->
            Result.map2 (\nest rest -> LDApply nest RAP :: rest) (transpile nested) (transpile xs)

        Prog.LDF :: (Prog.NESTED nested) :: xs ->
            Result.map2 (\nest rest -> LDLambda nest :: rest) (transpile nested) (transpile xs)

        Prog.RTN :: xs ->
            Result.map ((::) <| RTN) <| transpile xs

        Prog.SEL :: (Prog.NESTED nestedT) :: (Prog.NESTED nestedF) :: xs ->
            Result.map3 (\nestT nestF rest -> SEL nestT nestF :: rest) (transpile nestedT) (transpile nestedF) (transpile xs)

        Prog.JOIN :: xs ->
            Result.map ((::) <| JOIN) <| transpile xs

        Prog.DUM :: xs ->
            Result.map ((::) <| DUM) <| transpile xs

        (Prog.FUNC f) :: xs ->
            Result.map ((::) <| FUNC (Prog.funcToString f)) <| transpile xs

        Prog.AP :: xs ->
            Result.map ((::) <| LoneAP) <| transpile xs

        [] ->
            Ok []

        op :: _ ->
            Err <| "Unexpected op! " ++ Prog.opToString op



-- I need to beef up the program type (List Op) into something more helpful
-- Thus, I traverse the compiled ops and generate my own tree structure used just for a interactinv with the code
-- a lot of these changes can simply be built into the Program type itself, but I want to keep the program type as simple as possible


type Code
    = NIL
    | LD ( Int, Int )
    | LDC Int
    | LDFunc String -- Loads a function name (when a function is defined in a let stmt)
    | LDLambda (List Code) -- loads a lambda (when a lambda is an argument to a function)
    | LDApply (List Code) APType -- loads a function and apply it
    | LoneAP -- if a function is loaded from the env then applied
    | RTN
    | SEL (List Code) (List Code)
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
    Element.column
        []
        [ Debug.toString model.original
            |> Element.text
            |> List.singleton
            |> Element.paragraph []
        , case model.stuff of
            Err e ->
                Element.html <| Error.view e

            Ok okModel ->
                viewOk okModel
        ]


viewOk : OkModel -> Element Msg
viewOk model =
    Element.text "transpilation success c:"



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
