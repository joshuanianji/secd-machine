module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
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
import Lib.Colours as Colours
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util exposing (eachZero, eachZeroBorder)
import Lib.Views
import Ports
import SECD.Error exposing (Error)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
import Set exposing (Set)
import Url exposing (Url)
import UrlState exposing (UrlState)
import Views.Compiled as ViewCompiled
import Views.VM as ViewVM


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = viewApplication
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



---- MODEL ----


type alias Model =
    { state : UrlState
    , navKey : Nav.Key
    , data : ModelData
    }


type ModelData
    = Error Decode.Error
    | Success SuccessModel


type alias SuccessModel =
    { code : String

    -- open tabs in the code editor
    , openExampleTabs : Set String

    -- open tabs for the description
    , openTabs : Set String

    -- making this a result (super weird) so i can know when the code change
    -- is codemirror updating the code after the user clicks a new code example,
    -- or when the user edits the code
    -- when the user edits, we make the currCodeExample an empty string
    , currCodeExample : Result String String
    , compiled : CompiledState
    , codeExamples : CodeExamples

    -- default example name, code in case the url doesn't specify
    , defaultExample : Flags.DefaultExample
    }


type CompiledState
    = Idle
    | ParseError Error
    | CompileError AST Error
    | CompileSuccess AST ViewCompiled.Model ViewVM.Model


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialUrlState =
            UrlState.fromUrl url

        ( data, urlState, dataCmd ) =
            case Decode.decodeValue Flags.decoder flags of
                Err e ->
                    ( Error e, UrlState.default, Cmd.none )

                Ok f ->
                    let
                        ( initialTab, initialCode, urlState_ ) =
                            case Flags.findCodeExample initialUrlState.exampleName f.codeExamples of
                                Just ( exampleName, code ) ->
                                    ( exampleName, code, initialUrlState )

                                Nothing ->
                                    ( f.defaultExample.tab, f.defaultExample.code, UrlState.default )
                    in
                    ( Success
                        { code = initialCode
                        , openExampleTabs = Set.fromList [ initialTab ]
                        , openTabs = Set.fromList [ "howto" ]
                        , currCodeExample = Ok urlState_.exampleName
                        , compiled = Idle
                        , codeExamples = f.codeExamples
                        , defaultExample = f.defaultExample
                        }
                    , urlState_
                    , Ports.initialize initialCode
                    )
    in
    ( { state = urlState, navKey = key, data = data }, dataCmd )



---- MSG ----


type Msg
    = Remonke
    | ToggleTab String
    | ToggleExampleTab String
      -- code changed from JS side
    | CodeChanged String
      -- code changed from Elm side, arg is the example name
    | UpdateCodeExample String
    | Compile
    | ViewVMMsg ViewVM.Msg
    | ViewCompiledMsg ViewCompiled.Msg
      -- URL
    | ChangedUrl Url
    | ClickedLink UrlRequest
      -- other
    | UpdateScreen Screen



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.data, msg ) of
        ( Success m, ChangedUrl url ) ->
            let
                state =
                    UrlState.fromUrl url

                ( newTab, newCode, newState ) =
                    case Flags.findCodeExample state.exampleName m.codeExamples of
                        Just ( tab, code ) ->
                            ( tab, code, state )

                        -- use default
                        Nothing ->
                            ( m.defaultExample.tab, m.defaultExample.code, UrlState.default )

                newSuccessModel =
                    { m
                        | openExampleTabs = Set.fromList [ newTab ]
                        , currCodeExample = Ok newState.exampleName
                    }
            in
            ( { model
                | state = UrlState.merge newState model.state
                , data = Success newSuccessModel
              }
            , Ports.updateCode newCode
            )

        ( _, ClickedLink urlRequest ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        -- javascript will send us a codeChanged msg when CodeMirror changes their code.
        ( _, UpdateCodeExample name ) ->
            let
                ( newUrlState, cmd ) =
                    UrlState.updateTab name model.navKey model.state
            in
            ( { model | state = newUrlState }, cmd )

        ( Error _, _ ) ->
            ( model, Cmd.none )

        ( Success m, _ ) ->
            let
                ( newModel, newCmd ) =
                    updateSuccess msg m
            in
            ( { model | data = Success newModel }, newCmd )


updateSuccess : Msg -> SuccessModel -> ( SuccessModel, Cmd Msg )
updateSuccess msg model =
    case ( model.compiled, msg ) of
        ( _, Remonke ) ->
            ( model, Ports.initialize model.code )

        ( _, ToggleTab tab ) ->
            if Set.member tab model.openTabs then
                ( { model | openTabs = Set.remove tab model.openTabs }, Cmd.none )

            else
                ( { model | openTabs = Set.insert tab model.openTabs }, Cmd.none )

        ( _, ToggleExampleTab tab ) ->
            if Set.member tab model.openExampleTabs then
                ( { model | openExampleTabs = Set.remove tab model.openExampleTabs }, Cmd.none )

            else
                ( { model | openExampleTabs = Set.insert tab model.openExampleTabs }, Cmd.none )

        ( _, CodeChanged newCode ) ->
            case model.currCodeExample of
                Ok _ ->
                    ( { model | code = newCode }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | currCodeExample = Ok ""
                        , code = newCode
                      }
                    , Cmd.none
                    )

        ( _, Compile ) ->
            case AST.parse model.code of
                Err deadends ->
                    ( { model | compiled = ParseError <| Util.deadEndsToString deadends }, Cmd.none )

                Ok ast ->
                    case Prog.compile ast of
                        Err err ->
                            ( { model | compiled = CompileError ast err }, Cmd.none )

                        Ok prog ->
                            let
                                ( viewVMModel, viewVMMsg ) =
                                    ViewVM.init { maxPages = 15, pageSize = 10, chunkSize = 10 } prog

                                ( viewCompiledModel, viewCompiledMsg ) =
                                    ViewCompiled.init prog
                            in
                            ( { model | compiled = CompileSuccess ast viewCompiledModel viewVMModel }
                            , Cmd.batch [ Cmd.map ViewVMMsg viewVMMsg, Cmd.map ViewCompiledMsg viewCompiledMsg ]
                            )

        ( CompileSuccess ast compiledModel vmModel, ViewVMMsg subMsg ) ->
            let
                ( newVMModel, newVMMsg ) =
                    ViewVM.update subMsg vmModel
            in
            ( { model | compiled = CompileSuccess ast compiledModel newVMModel }, Cmd.map ViewVMMsg newVMMsg )

        ( CompileSuccess ast compiledModel vmModel, ViewCompiledMsg subMsg ) ->
            let
                ( newCompiledModel, newCompiledMsg ) =
                    ViewCompiled.update subMsg compiledModel
            in
            ( { model | compiled = CompileSuccess ast newCompiledModel vmModel }, Cmd.map ViewCompiledMsg newCompiledMsg )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


viewApplication : Model -> Browser.Document Msg
viewApplication model =
    { title = "SECD Machine: " ++ model.state.exampleName
    , body =
        [ view model ]
    }


view : Model -> Html Msg
view model =
    case model.data of
        Error e ->
            Html.text <| Decode.errorToString e

        Success m ->
            viewSuccess m


viewSuccess : SuccessModel -> Html Msg
viewSuccess model =
    let
        title =
            Element.el
                [ Font.size 36
                , Font.bold
                , Element.centerX
                , Element.paddingXY 0 16
                ]
                (Element.text "SECD Machine")

        subtitle =
            Element.el
                [ Font.size 18
                , Font.bold
                , Element.centerX
                , Element.paddingXY 0 8
                ]
                (Element.text "An implementation as seen in Ualberta's CMPUT 325")

        parseBtns =
            Element.row
                [ Element.spacing 8
                , Element.centerX
                ]
                [ Lib.Views.button Remonke <| Element.text "Rerun Monkey"
                , Lib.Views.button Compile <| Element.text "Parse + Compile"
                ]

        vmEditor =
            case model.compiled of
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

                CompileError _ err ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.size 24, Font.bold ] <| Element.text "Compile error!"
                        , Element.paragraph [ Font.size 16 ] [ Element.text err ]
                        ]

                CompileSuccess _ compiledModel vmModel ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 32
                        ]
                        [ Element.map ViewCompiledMsg <| ViewCompiled.view compiledModel
                        , Element.map ViewVMMsg <| ViewVM.view vmModel
                        ]

        footer =
            Element.column
                [ Element.centerX
                , Element.paddingXY 0 96
                , Element.spacing 32
                ]
                -- mimic an <hr />
                [ Element.el
                    [ Element.width Element.fill
                    , Element.height Element.shrink
                    , Border.width 1
                    , Border.color <| Colours.greyAlpha 0.2
                    ]
                    Element.none
                , Element.el [ Element.centerX ] <| Element.text "Made with ♥ by Joshua Ji"
                ]
    in
    Element.column
        [ Element.height Element.fill
        , Element.htmlAttribute <| Html.Attributes.id "main-content"
        , Element.spacing 8
        , Element.paddingXY 24 0
        , Element.centerX
        ]
        [ title
        , subtitle
        , description model
        , codeEditor model
        , parseBtns
        , vmEditor
        , footer
        ]
        |> Element.layoutWith
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
            , Element.width Element.fill
            , Element.height Element.fill
            ]



-- contains the description of the SECD machine and stuff


description : SuccessModel -> Element Msg
description model =
    Element.column
        [ Element.spacing 32
        , Element.paddingXY 0 24
        , Element.width Element.fill
        ]
        [ Lib.Views.viewTogglable []
            { title = "What is an SECD Machine?"
            , activeWhen = Set.member "whatis" model.openTabs
            , onClick = ToggleTab "whatis"
            , body = descriptionWhatis
            }
        , Lib.Views.viewTogglable []
            { title = "How does this work?"
            , activeWhen = Set.member "howto" model.openTabs
            , onClick = ToggleTab "howto"
            , body = descriptionHowto
            }
        ]


descriptionWhatis : Element Msg
descriptionWhatis =
    Element.column
        [ Element.spacing 12
        , Element.width Element.fill
        ]
        [ Element.paragraph
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.text "An SECD is a virtual machine designed primarily for running compiled code from functional languages.  It consists of four stacks: the "
            , Lib.Views.bold "S"
            , Element.text "tack, holding the 'values' of the executed code, "
            , Lib.Views.bold "E"
            , Element.text "nvironment, containing values of the current scope, "
            , Lib.Views.bold "C"
            , Element.text "ontrol, containing instructions to the machine, and "
            , Lib.Views.bold "D"
            , Element.text "ump stack, used for temporary storage."
            ]
        , Element.paragraph
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.text "The compiled code or instruction set, on the other hand, is relatively simple. For brevity, I won't state them here, nor expand on the SECD definition, but feel free to check the references below:" ]
        , Element.column
            [ Element.paddingXY 8 4
            , Element.spacing 8
            ]
          <|
            List.map
                (\( url, label ) ->
                    Element.row
                        [ Element.spacing 8 ]
                        [ Element.el [ Lib.Views.unselectable ] <| Element.text "-"
                        , Lib.Views.link [] { url = url, label = label }
                        ]
                )
                [ ( "https://webdocs.cs.ualberta.ca/~rgreiner/C-325/2004/325/2004/Slides/HandoutPDF/SECD-1x2.pdf"
                  , "SECD Notes from UAlberta CMPUT 325 - November 2004"
                  )
                , ( "https://en.wikipedia.org/wiki/SECD_machine", "Wikipedia Article" )
                , ( "https://github.com/zachallaun/secd", "Zachallaun's implementation" )
                ]
        ]


descriptionHowto : Element Msg
descriptionHowto =
    Element.column
        [ Element.spacing 12
        , Element.width Element.fill
        ]
        [ Element.paragraph
            [ Element.spacing 8
            ]
            [ Element.text "This implementation has two interactive parts, the \"code editor\", for a lisp-like language, and the \"machine\", which executes the compiled code."
            ]
        , Element.paragraph
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.text "To glue those two parts together, I split my program into three sections: "
            , Lib.Views.bold "parsing"
            , Element.text " my code into an AST, "
            , Lib.Views.bold "compiling"
            , Element.text " the AST into the SECD instruction set, then "
            , Lib.Views.bold "running"
            , Element.text " it on the VM."
            ]
        ]



-- contains the code editor and the code editor tabs


codeEditor : SuccessModel -> Element Msg
codeEditor model =
    let
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
                                if Set.member name model.openExampleTabs then
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
                                , Events.onClick <| ToggleExampleTab name
                                , Element.pointer
                                , Element.spacing 4
                                , Lib.Views.unselectable
                                , Element.mouseOver
                                    [ Background.color Colours.slateGrey ]
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
                        Colours.purple

                    else
                        Colours.white
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
                        [ Font.color Colours.lightGrey ]
                    ]
                    { onPress = Just <| UpdateCodeExample name
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
            , Background.color Colours.grey
            , Element.padding 16
            ]
            (Element.html <| Html.div [ Html.Attributes.id "editor" ] [])
        , Element.el
            [ Element.width <| Element.fillPortion 2
            , Element.height <| Element.px 500
            , Element.scrollbars
            , Border.width 2
            , Border.roundEach { eachZeroBorder | topRight = 16, bottomRight = 16 }
            , Border.color Colours.grey
            ]
            (viewCodeExamples model.codeExamples)
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.data of
        Error _ ->
            Sub.none

        Success m ->
            let
                subModelSubscriptions =
                    case m.compiled of
                        CompileSuccess _ compiledModel vmModel ->
                            Sub.batch
                                [ Sub.map ViewVMMsg <| ViewVM.subscriptions vmModel
                                , Sub.map ViewCompiledMsg <| ViewCompiled.subscriptions compiledModel
                                ]

                        _ ->
                            Sub.none
            in
            Sub.batch
                [ subModelSubscriptions
                , Ports.updatedEditor CodeChanged
                ]
