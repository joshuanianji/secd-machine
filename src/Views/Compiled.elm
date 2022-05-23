module Views.Compiled exposing (Model, Msg, init, subscriptions, update, view)

-- | Compiled View
-- Views the compiled code.

import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
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
    { code : List Prog.Op }


init : Program -> ( Model, Cmd Msg )
init prog =
    ( { code = Prog.toList prog }
    , Cmd.none
    )



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
    Element.text "hi"



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
