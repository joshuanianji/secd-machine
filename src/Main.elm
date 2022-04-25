module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import List.Zipper as Zipper exposing (Zipper)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
import SECD.VM as VM


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { states : Zipper VM.State
    , pressedKeys : List Key
    }


init : ( Model, Cmd Msg )
init =
    let
        vm =
            VM.initRaw factorial

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
                ( newModel, Cmd.none )



-- recursively calculates the length of a list
{--
(letrec (f (λx m | (if (null x) m (f (cdr x) (+ m 1) )) ) )
(f '(1 2 3) 0) )
--}


recLength : Prog.Program
recLength =
    let
        func =
            [ LD ( 0, 0 ), FUNC NULL, SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ NIL, LDC 1, LD ( 0, 1 ), FUNC ADD, FUNC CONS, LD ( 0, 0 ), FUNC CDR, FUNC CONS, LD ( 1, 0 ), AP, JOIN ], RTN ]

        -- (f '(1 2 3) 0)
        funcApply =
            [ NIL, LDC 0, FUNC CONS, NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC CONS, LD ( 0, 0 ), AP, RTN ]
    in
    Prog.fromList
        [ DUM, NIL, LDF, NESTED func, FUNC CONS, LDF, NESTED funcApply, RAP ]


factorial : Prog.Program
factorial =
    let
        fact =
            [ LDC 0, LD ( 0, 0 ), FUNC (COMPARE CMP_EQ), SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ NIL, LD ( 0, 1 ), LD ( 0, 0 ), FUNC MULT, FUNC CONS, LD ( 2, 1 ), LD ( 0, 0 ), FUNC SUB, FUNC CONS, LD ( 1, 0 ), AP, JOIN ], RTN ]

        -- I actually don't really know what this does
        factCreateClosure =
            [ NIL, LD ( 1, 1 ), FUNC CONS, LD ( 1, 0 ), FUNC CONS, LD ( 0, 0 ), AP, RTN ]
    in
    Prog.fromList [ NIL, LDC 1, FUNC CONS, LDC 6, FUNC CONS, LDF, NESTED [ DUM, NIL, LDF, NESTED fact, FUNC CONS, LDF, NESTED factCreateClosure, RAP, RTN ], AP ]


letLambda : Prog.Program
letLambda =
    Prog.fromList [ NIL, LDF, NESTED [ LDC 1, LD ( 0, 0 ), FUNC ADD, RTN ], FUNC CONS, LDF, NESTED [ NIL, LDC 3, FUNC CONS, LD ( 0, 0 ), AP, RTN ], AP ]



-- I'm not sure how mutually recursive functions work, so let's try it out!


mutuallyRecursiveIsEven : Int -> Prog.Program
mutuallyRecursiveIsEven n =
    let
        isEven =
            mutualRecursive [ NIL ] ( 1, 1 )

        isOdd =
            mutualRecursive [ NIL, FUNC ATOM ] ( 1, 0 )

        mutualRecursive onTrue letrecCoords =
            [ LDC 0, LD ( 0, 0 ), FUNC (COMPARE CMP_EQ), SEL, NESTED <| onTrue ++ [ JOIN ], NESTED [ NIL, LDC 1, LD ( 0, 0 ), FUNC SUB, FUNC CONS, LD letrecCoords, AP, JOIN ], RTN ]

        body =
            [ NIL, LDC n, FUNC CONS, LD ( 0, 1 ), AP, RTN ]
    in
    Prog.fromList
        [ DUM, NIL, LDF, NESTED isOdd, FUNC CONS, LDF, NESTED isEven, FUNC CONS, LDF, NESTED body, RAP ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ Attr.class "container" ]
        [ div [ Attr.class "btns" ]
            [ button [ Events.onClick First ] [ text "first" ]
            , button [ Events.onClick Previous ] [ text "previous" ]
            , button [ Events.onClick Step ] [ text "step" ]
            , button [ Events.onClick Last ] [ text "last" ]
            ]
        , VM.viewState 6 <| Zipper.current model.states
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyMsg Keyboard.subscriptions
