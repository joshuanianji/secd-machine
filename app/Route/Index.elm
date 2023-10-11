module Route.Index exposing (Model, Msg, RouteParams, route, Data, ActionData)

{-|

@docs Model, Msg, RouteParams, route, Data, ActionData

-}

import Backend.GetEnv
import Backend.GetExamplesTask exposing (Example, ExampleGroup)
import BackendTask exposing (BackendTask)
import Dict
import Effect
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ErrorPage
import FatalError exposing (FatalError)
import FeatherIcons
import Head
import Html
import Html.Attributes
import LanguageTag.Country exposing (n_001)
import Lib.Colours as Colours
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util exposing (eachZero, eachZeroBorder)
import Lib.Views
import List.Nonempty exposing (Nonempty)
import Pages.PageUrl
import PagesMsg
import Ports
import RouteBuilder
import SECD.Error exposing (Error)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
import Scaffold.Route exposing (Type(..))
import Set exposing (Set)
import Shared
import UrlPath
import View
import Views.Compiled as ViewCompiled
import Views.VM as ViewVM


type alias Model =
    { code : String

    -- open tabs in the code editor
    , openExampleTabs : Set String

    -- open tabs for the description
    , openTabs : Set String
    , currCodeExample : CodeExample
    , compiled : CompiledState
    }



-- When we get a code change from JS, it can be in one of three states:
--  * User edits the code - the code example should now be none!
--  * User just clicked a new code example tab, and CodeMirror is telling us the code finished updating
-- In the second example, we want to remember that the code example is still the same!


type
    CodeExample
    -- If we get a `CodeChanged` msg, we know this is just CodeMirror updating
    = Updating String
      -- if we get a `CodeChanged` msg, we know the user has changed the code
    | Stable String
    | CustomUserCode


getCurrentCodeExample : CodeExample -> String
getCurrentCodeExample codeExample =
    case codeExample of
        Updating name ->
            name

        Stable name ->
            name

        CustomUserCode ->
            "Custom User Code"


type CompiledState
    = Idle
    | ParseError Error
    | CompileError AST Error
    | CompileSuccess AST ViewCompiled.Model ViewVM.Model


type alias RouteParams =
    {}


route : RouteBuilder.StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.preRender
        { data = \_ -> data
        , head = head
        , pages = BackendTask.succeed [ {} ]
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , init = init
            , update = update
            , subscriptions = subscriptions
            }


init :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> ( Model, Effect.Effect Msg )
init app _ =
    let
        -- find example in app.data.exampleGroups
        ( defaultExampleTab, defaultExample ) =
            Backend.GetExamplesTask.getDefault app.data.exampleGroups
    in
    ( { code = defaultExample.code
      , openExampleTabs = Set.fromList [ defaultExampleTab ]
      , openTabs = Set.fromList [ "howto" ]
      , currCodeExample = Updating defaultExample.id
      , compiled = Idle
      }
    , Effect.fromCmd <| Ports.initialize defaultExample.code
    )


type Msg
    = Remonke
    | ToggleTab String
    | ToggleExampleTab String
      -- User changed code example
      -- arg is the `id` `code`
    | ChangeCodeExample String String
      -- code changed from JS side
    | CodeChanged String
      -- code changed from Elm side, arg is the example name
    | Compile
    | ViewVMMsg ViewVM.Msg
    | ViewCompiledMsg ViewCompiled.Msg


update :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> ( Model, Effect.Effect Msg )
update _ _ msg model =
    case ( model.compiled, msg ) of
        ( _, Remonke ) ->
            ( model, Effect.fromCmd <| Ports.initialize model.code )

        ( _, ToggleTab tab ) ->
            if Set.member tab model.openTabs then
                ( { model | openTabs = Set.remove tab model.openTabs }, Effect.none )

            else
                ( { model | openTabs = Set.insert tab model.openTabs }, Effect.none )

        ( _, ToggleExampleTab tab ) ->
            if Set.member tab model.openExampleTabs then
                ( { model | openExampleTabs = Set.remove tab model.openExampleTabs }, Effect.none )

            else
                ( { model | openExampleTabs = Set.insert tab model.openExampleTabs }, Effect.none )

        ( _, CodeChanged newCode ) ->
            case model.currCodeExample of
                Updating id ->
                    ( { model
                        | currCodeExample = Stable id
                        , code = newCode
                      }
                    , Effect.none
                    )

                Stable _ ->
                    ( { model
                        | currCodeExample = CustomUserCode
                        , code = newCode
                      }
                    , Effect.none
                    )

                CustomUserCode ->
                    ( { model | code = newCode }, Effect.none )

        -- javascript will send us a codeChanged msg when CodeMirror changes their code.
        ( _, ChangeCodeExample id code ) ->
            ( { model | currCodeExample = Updating id }, Effect.fromCmd <| Ports.updateCode code )

        ( _, Compile ) ->
            case AST.parse model.code of
                Err deadends ->
                    ( { model | compiled = ParseError <| Util.deadEndsToString deadends }, Effect.none )

                Ok ast ->
                    case Prog.compile ast of
                        Err err ->
                            ( { model | compiled = CompileError ast err }, Effect.none )

                        Ok prog ->
                            let
                                ( viewVMModel, viewVMMsg ) =
                                    ViewVM.init { maxPages = 15, pageSize = 10, chunkSize = 10 } prog

                                ( viewCompiledModel, viewCompiledMsg ) =
                                    ViewCompiled.init prog
                            in
                            ( { model | compiled = CompileSuccess ast viewCompiledModel viewVMModel }
                            , [ Cmd.map ViewVMMsg viewVMMsg, Cmd.map ViewCompiledMsg viewCompiledMsg ]
                                |> Cmd.batch
                                |> Effect.fromCmd
                            )

        ( CompileSuccess ast compiledModel vmModel, ViewVMMsg subMsg ) ->
            let
                ( newVMModel, newVMMsg ) =
                    ViewVM.update subMsg vmModel
            in
            ( { model | compiled = CompileSuccess ast compiledModel newVMModel }
            , Cmd.map ViewVMMsg newVMMsg
                |> Effect.fromCmd
            )

        ( CompileSuccess ast compiledModel vmModel, ViewCompiledMsg subMsg ) ->
            let
                ( newCompiledModel, newCompiledMsg ) =
                    ViewCompiled.update subMsg compiledModel
            in
            ( { model | compiled = CompileSuccess ast newCompiledModel vmModel }
            , Cmd.map ViewCompiledMsg newCompiledMsg
                |> Effect.fromCmd
            )

        _ ->
            ( model, Effect.none )


type alias Data =
    { exampleGroups : Nonempty Backend.GetExamplesTask.ExampleGroup
    , env : Backend.GetEnv.Env
    }


type alias ActionData =
    {}


data : BackendTask FatalError Data
data =
    BackendTask.succeed Data
        |> BackendTask.andMap Backend.GetExamplesTask.examples
        |> BackendTask.andMap Backend.GetEnv.retrieve


head : RouteBuilder.App Data ActionData RouteParams -> List Head.Tag
head app =
    []


view :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View.View (PagesMsg.PagesMsg Msg)
view app shared model =
    { title = "SECD Machine"
    , body =
        viewApp app model
            |> Element.map PagesMsg.fromMsg
    }


viewApp : RouteBuilder.App Data ActionData RouteParams -> Model -> Element Msg
viewApp app model =
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
                [ if app.data.env.mode == Backend.GetEnv.Prod then
                    Element.none

                  else
                    Lib.Views.button Remonke <| Element.text "Rerun Monkey"
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
                , Element.spacing 24
                ]
                -- mimic an <hr />
                [ Element.el
                    [ Element.width Element.fill
                    , Element.height Element.shrink
                    , Border.width 1
                    , Border.color <| Colours.greyAlpha 0.2
                    ]
                    Element.none
                , Element.paragraph
                    []
                    [ Element.text "Source code is on "
                    , Lib.Views.link []
                        { url = "https://github.com/joshuanianji/secd-machine"
                        , label = "GitHub"
                        }
                    ]
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
        , codeEditor model app.data.exampleGroups
        , parseBtns
        , vmEditor
        , footer
        ]



-- description of the SECD machine


description : Model -> Element Msg
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


codeEditor : Model -> Nonempty ExampleGroup -> Element Msg
codeEditor model exampleGroups =
    let
        viewCodeExamples : Nonempty ExampleGroup -> Element Msg
        viewCodeExamples =
            List.Nonempty.map
                (\exGroup ->
                    let
                        ( icon, content ) =
                            if Set.member exGroup.groupName model.openExampleTabs then
                                ( FeatherIcons.chevronUp
                                , Element.column
                                    [ Element.paddingEach { eachZero | left = 16 }
                                    , Element.width Element.fill
                                    ]
                                    (List.Nonempty.map viewCodeExampleTab exGroup.examples
                                        |> List.Nonempty.toList
                                    )
                                )

                            else
                                ( FeatherIcons.chevronDown, Element.none )
                    in
                    Element.column
                        [ Element.width Element.fill ]
                        [ Element.row
                            [ Element.width Element.fill
                            , Element.paddingXY 16 12
                            , Events.onClick <| ToggleExampleTab exGroup.groupName
                            , Element.pointer
                            , Element.spacing 4
                            , Lib.Views.unselectable
                            , Element.mouseOver
                                [ Background.color Colours.slateGrey ]
                            ]
                            [ Element.text exGroup.groupName
                            , Util.viewIcon [ Element.alignRight ] icon
                            ]
                        , content
                        ]
                )
                >> List.Nonempty.toList
                >> Element.column
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.paddingXY 0 12
                    ]

        viewCodeExampleTab : Example -> Element Msg
        viewCodeExampleTab { id, name, code } =
            let
                dotColor =
                    if id == getCurrentCodeExample model.currCodeExample then
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
                    { onPress = Just <| ChangeCodeExample id code
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
            (viewCodeExamples exampleGroups)
        ]


subscriptions : RouteParams -> UrlPath.UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ model =
    let
        subModelSubscriptions =
            case model.compiled of
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
