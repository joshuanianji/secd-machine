module VMView exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import List.Zipper as Zipper exposing (Zipper)
import SECD.Program exposing (Program)
import SECD.VM as VM



---- MODEL ----


type alias Model =
    { states : Zipper VM.State
    , pressedKeys : List Key
    }


init : Program -> Model
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
    { states = Zipper.from [] currState afters
    , pressedKeys = []
    }



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


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ Html.div [ Attr.class "btns" ]
            [ Html.button [ Events.onClick First ] [ Html.text "first" ]
            , Html.button [ Events.onClick Previous ] [ Html.text "previous" ]
            , Html.button [ Events.onClick Step ] [ Html.text "step" ]
            , Html.button [ Events.onClick Last ] [ Html.text "last" ]
            ]
        , VM.viewState 6 <| Zipper.current model.states
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyMsg Keyboard.subscriptions
