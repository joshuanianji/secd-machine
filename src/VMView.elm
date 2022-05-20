module VMView exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Lib.Views
import List.Zipper as Zipper exposing (Zipper)
import SECD.Program exposing (Program)
import SECD.VM as VM



---- MODEL ----


type alias Model =
    { states : Zipper VM.State

    -- each "page" holds 100 VM states
    -- the "states" hold the current 100 states of the VM
    -- the rest are held in SessionStorage
    , pages : Zipper Int
    , pressedKeys : List Key
    }


init : Program -> ( Model, Cmd Msg )
init prog =
    let
        vm =
            VM.initRaw prog

        currState =
            VM.initState vm

        afters =
            case currState of
                VM.Unfinished nextState ->
                    VM.evalList nextState

                _ ->
                    []
    in
    ( { states = Zipper.from [] currState afters
      , pages = Zipper.from [] 0 []
      , pressedKeys = []
      }
    , Cmd.none
    )



---- MSG ----


type Msg
    = First
    | Previous
    | Step
    | Last
    | KeyMsg Keyboard.Msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        First ->
            ( { model | states = Zipper.first model.states }, Cmd.none )

        Previous ->
            case Zipper.previous model.states of
                Nothing ->
                    ( model, Cmd.none )

                Just newState ->
                    ( { model | states = newState }, Cmd.none )

        Step ->
            case Zipper.next model.states of
                Nothing ->
                    ( model, Cmd.none )

                Just newState ->
                    ( { model | states = newState }, Cmd.none )

        Last ->
            ( { model | states = Zipper.last model.states }, Cmd.none )

        KeyMsg keyMsg ->
            let
                newPressedKeys =
                    Keyboard.update keyMsg model.pressedKeys

                newModel =
                    { model | pressedKeys = newPressedKeys }

                arrows =
                    Keyboard.Arrows.arrows newPressedKeys
            in
            if arrows.x == 1 then
                -- right key
                update Step newModel

            else if arrows.x == -1 then
                update Previous newModel

            else
                ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 8
        , Element.spacingXY 8 12
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Lib.Views.button First <| Element.text "First"
            , Lib.Views.button Previous <| Element.text "Previous"
            , Lib.Views.button Step <| Element.text "Step"
            , Lib.Views.button Last <| Element.text "Last"
            ]
        , Element.html <| VM.viewState 6 <| Zipper.current model.states
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyMsg Keyboard.subscriptions
