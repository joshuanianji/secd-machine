module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Flags exposing (CodeExamples, Screen)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util exposing (eachZero, eachZeroBorder)
import Lib.Views
import Ports
import SECD.Error exposing (Error)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
import Set exposing (Set)
import VMView


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



---- MODEL ----


type Model
    = Error Decode.Error
    | Success SuccessModel


type alias SuccessModel =
    { code : String
    , openTabs : Set String

    -- making this a result (super weird) so i can know when the code change
    -- is codemirror updating the code after the user clicks a new code example,
    -- or when the user edits the code
    -- when the user edits, we make the currCodeExample an empty string
    , currCodeExample : Result String String
    , compiled : CompiledState
    , codeExamples : CodeExamples
    , screen : Screen
    }


type CompiledState
    = Idle
    | ParseError Error
    | CompileError AST Error
    | CompileSuccess AST VMView.Model


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue Flags.decoder flags of
        Err e ->
            ( Error e, Cmd.none )

        Ok f ->
            ( Success
                { code = f.initialCode
                , openTabs = Set.fromList [ "Basics" ]
                , currCodeExample = Ok "Arithmetic"
                , compiled = Idle
                , codeExamples = f.codeExamples
                , screen = f.screen
                }
            , Ports.initialized f.initialCode
            )



---- MSG ----


type Msg
    = Remonke
    | ToggleTab String
      -- code changed from JS side
    | CodeChanged String
      -- code changed from Elm side
      -- first arg is name, second is the code
    | UpdateCodeExample String String
    | Compile
    | VMViewMsg VMView.Msg
      -- other
    | UpdateScreen Screen



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Error _ ->
            ( model, Cmd.none )

        Success m ->
            updateSuccess msg m
                |> Tuple.mapFirst Success


updateSuccess : Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateSuccess msg model =
    case ( model.compiled, msg ) of
        ( _, Remonke ) ->
            ( model, Ports.initialized model.code )

        ( _, ToggleTab tab ) ->
            if Set.member tab model.openTabs then
                ( { model | openTabs = Set.remove tab model.openTabs }, Cmd.none )

            else
                ( { model | openTabs = Set.insert tab model.openTabs }, Cmd.none )

        ( _, CodeChanged newCode ) ->
            case model.currCodeExample of
                Ok codeChangedExample ->
                    ( { model
                        | currCodeExample = Err codeChangedExample
                        , code = newCode
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | currCodeExample = Ok ""
                        , code = newCode
                      }
                    , Cmd.none
                    )

        -- javascript will send us a codeChanged msg when CodeMirror changes their code.
        ( _, UpdateCodeExample name newCode ) ->
            ( { model | currCodeExample = Ok name }, Ports.updateCode newCode )

        ( _, Compile ) ->
            case AST.parse model.code of
                Err deadends ->
                    ( { model | compiled = ParseError <| Util.deadEndsToString deadends }, Cmd.none )

                Ok ast ->
                    case Prog.compile ast of
                        Err err ->
                            ( { model | compiled = CompileError ast err }, Cmd.none )

                        Ok prog ->
                            ( { model | compiled = CompileSuccess ast (VMView.init prog) }, Cmd.none )

        ( CompileSuccess ast vmModel, VMViewMsg subMsg ) ->
            let
                ( newVMModel, newVMMsg ) =
                    VMView.update subMsg vmModel
            in
            ( { model | compiled = CompileSuccess ast newVMModel }, Cmd.map VMViewMsg newVMMsg )

        ( _, UpdateScreen newScreen ) ->
            ( { model | screen = newScreen }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Error e ->
            Html.text <| Decode.errorToString e

        Success m ->
            viewSuccess m


viewSuccess : SuccessModel -> Html Msg
viewSuccess model =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Font.family
            [ Font.typeface "Avenir"
            , Font.typeface "Helvetica"
            , Font.typeface "Arial"
            , Font.sansSerif
            ]
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 8
            ]
            [ Element.el
                [ Font.size 36
                , Font.bold
                , Element.centerX
                , Element.paddingXY 0 16
                ]
              <|
                Element.text "SECD Machine"
            , Element.el
                [ Font.size 18
                , Font.bold
                , Element.centerX
                , Element.paddingXY 0 8
                ]
              <|
                Element.text "An implementation as seen in Ualberta's CMPUT 325"
            , codeEditor model
            , Element.row
                [ Element.spacing 8
                , Element.centerX
                ]
                [ Lib.Views.button Remonke <| Element.text "Rerun Monkey"
                , Lib.Views.button Compile <| Element.text "Parse + Compile"
                ]
            , case model.compiled of
                Idle ->
                    Element.none

                ParseError err ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.size 24 ] <| Element.text "Parse error!"
                        , Element.paragraph [ Font.size 16 ] [ Element.text err ]
                        ]

                CompileError ast err ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.size 24, Font.bold ] <| Element.text "Parse success"
                        , Element.paragraph [ Font.size 16 ] [ Element.text <| Debug.toString ast ]
                        , Element.el [ Font.size 24, Font.bold ] <| Element.text "Compile error!"
                        , Element.paragraph [ Font.size 16 ] [ Element.text err ]
                        ]

                CompileSuccess ast vmModel ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.size 24, Font.bold ] <| Element.text "Parse success"
                        , Element.paragraph [ Font.size 16 ] [ Element.text <| Debug.toString ast ]
                        , Element.el [ Font.size 24, Font.bold ] <| Element.text "Compilation success!"
                        , Element.map VMViewMsg <| VMView.view vmModel
                        ]
            ]


codeEditor : SuccessModel -> Element Msg
codeEditor model =
    let
        ( surroundSize, mainSize ) =
            case .class (Element.classifyDevice model.screen) of
                Element.Desktop ->
                    ( 1, 5 )

                Element.Phone ->
                    ( 1, 10 )

                Element.Tablet ->
                    ( 1, 8 )

                Element.BigDesktop ->
                    ( 1, 3 )

        viewCodeExamples : CodeExamples -> Element Msg
        viewCodeExamples progs =
            Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.paddingXY 0 12
                ]
            <|
                List.map
                    (\( name, progTuples ) ->
                        let
                            ( icon, content ) =
                                if Set.member name model.openTabs then
                                    ( FeatherIcons.chevronUp
                                    , Element.column
                                        [ Element.paddingEach { eachZero | left = 16 }
                                        , Element.width Element.fill
                                        ]
                                        (List.map viewCodeExampleTab progTuples)
                                    )

                                else
                                    ( FeatherIcons.chevronDown, Element.none )
                        in
                        Element.column
                            [ Element.width Element.fill ]
                            [ Element.row
                                [ Element.width Element.fill
                                , Element.paddingXY 16 12
                                , Events.onClick <| ToggleTab name
                                , Element.pointer
                                , Element.spacing 4
                                , Element.htmlAttribute <| Html.Attributes.class "unselectable"
                                , Element.mouseOver
                                    [ Background.color (Element.rgba255 38 50 56 0.1) ]
                                ]
                                [ Element.text name
                                , Util.viewIcon [ Element.alignRight ] icon
                                ]
                            , content
                            ]
                    )
                    progs

        viewCodeExampleTab : ( String, String ) -> Element Msg
        viewCodeExampleTab ( name, prog ) =
            let
                dotColor =
                    if name == Util.foldResult model.currCodeExample then
                        Element.rgb255 199 146 234

                    else
                        Element.rgba 0 0 0 0
            in
            Element.row
                [ Element.width Element.fill
                , Element.spacing 4
                ]
                [ Element.el
                    [ Element.height <| Element.px 10
                    , Element.width <| Element.px 10
                    , Background.color dotColor
                    , Element.centerY
                    , Border.rounded 16
                    ]
                    Element.none
                , Input.button
                    [ Element.padding 12
                    , Element.width Element.fill
                    , Element.mouseOver
                        [ Font.color (Element.rgba255 38 50 56 0.8) ]
                    ]
                    { onPress = Just <| UpdateCodeExample name prog
                    , label = Element.text name
                    }
                ]
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.el
            [ Element.width <| Element.fillPortion 5
            , Element.height <| Element.px 500
            , Border.roundEach { eachZeroBorder | topLeft = 16, bottomLeft = 16 }
            , Background.color <| Element.rgb255 38 50 56
            , Element.padding 16
            ]
          <|
            Element.html (Html.div [ Html.Attributes.id "editor" ] [])
        , Element.el
            [ Element.width <| Element.fillPortion 2
            , Element.height <| Element.px 500
            , Element.scrollbars
            , Border.width 2
            , Border.roundEach { eachZeroBorder | topRight = 16, bottomRight = 16 }
            , Border.color <| Element.rgb255 38 50 56
            ]
          <|
            viewCodeExamples model.codeExamples
        ]
        |> Util.surround
            [ Element.paddingXY 0 24 ]
            { left = surroundSize
            , middle = mainSize
            , right = surroundSize
            }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Error _ ->
            Sub.none

        Success m ->
            let
                vmModelSub =
                    case m.compiled of
                        CompileSuccess _ vmModel ->
                            VMView.subscriptions vmModel

                        _ ->
                            Sub.none
            in
            Sub.batch
                [ Sub.map VMViewMsg vmModelSub
                , Ports.updatedEditor CodeChanged
                , Browser.Events.onResize (\w h -> UpdateScreen <| Screen w h)
                ]
