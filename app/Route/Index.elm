module Route.Index exposing (Model, Msg, RouteParams, route, Data, ActionData)

{-|

@docs Model, Msg, RouteParams, route, Data, ActionData

-}

import Backend.GetExamplesTask
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
import Head
import Html
import Html.Attributes
import Lib.Colours as Colours
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util exposing (eachZero, eachZeroBorder)
import Lib.Views
import List.Nonempty as Nonempty exposing (Nonempty)
import Pages.PageUrl
import PagesMsg
import Ports
import RouteBuilder
import SECD.Error exposing (Error)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
import Server.Request
import Server.Response
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

    -- making this a result (super weird) so i can know when the code change
    -- is codemirror updating the code after the user clicks a new code example,
    -- or when the user edits the code
    -- when the user edits, we make the currCodeExample an empty string
    , currCodeExample : Result String String
    , compiled : CompiledState
    }


type CompiledState
    = Idle
    | ParseError Error
    | CompileError AST Error
    | CompileSuccess AST ViewCompiled.Model ViewVM.Model


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
init app shared =
    let
        openedExample =
            Maybe.map .query app.url
                |> Maybe.andThen (Dict.get "example")

        _ =
            Debug.log "openedExample" openedExample
    in
    ( { code = ""
      , openExampleTabs = Set.fromList []
      , openTabs = Set.fromList [ "howto" ]
      , currCodeExample = Result.Err ""
      , compiled = Idle
      }
    , Effect.none
    )


update :
    RouteBuilder.App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> ( Model, Effect.Effect Msg )
update app shared msg model =
    case msg of
        _ ->
            ( model, Effect.none )


subscriptions : RouteParams -> UrlPath.UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none


type alias Data =
    { exampleGroups : Nonempty Backend.GetExamplesTask.ExampleGroup
    }


type alias ActionData =
    {}


data : BackendTask FatalError Data
data =
    BackendTask.succeed Data
        |> BackendTask.andMap Backend.GetExamplesTask.examples


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

        -- , codeEditor model
        , parseBtns

        -- , vmEditor
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


action :
    RouteParams
    -> Server.Request.Request
    -> BackendTask.BackendTask FatalError.FatalError (Server.Response.Response ActionData ErrorPage.ErrorPage)
action routeParams request =
    BackendTask.succeed (Server.Response.render {})
