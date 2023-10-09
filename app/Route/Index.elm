module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html
import Pages.Url
import PagesMsg exposing (PagesMsg)
import UrlPath
import Route
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)
import Element exposing (Element)
import Backend.GetExamplesTask
import List.Nonempty as Nonempty exposing (Nonempty)
import Lib.Colours as Colours
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util exposing (eachZero, eachZeroBorder)
import Lib.Views
import Ports
import SECD.Error exposing (Error)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
import UrlState exposing (UrlState)
import Views.Compiled as ViewCompiled
import Views.VM as ViewVM
import Set

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


type alias Data =
    { exampleGroups : Nonempty Backend.GetExamplesTask.ExampleGroup
    }


type alias ActionData =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }

data : BackendTask FatalError Data
data =
    BackendTask.succeed Data
        |> BackendTask.andMap Backend.GetExamplesTask.examples


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "secd-machine"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "SECD Machine on the web"
        , locale = Nothing
        , title = "SECD Machine"
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app shared =
    { title = "SECD Machine"
    , body =
        Element.column 
            []
            [Element.text "Hello!!!"
            , Element.column 
                [Element.spacing 10]
                <|
                List.map 
                    (\group -> 
                        Element.row 
                            []
                            [Element.text group.groupName
                            , Element.column 
                            []
                            <| 
                            List.map 
                                (\example -> 
                                    Element.row 
                                        []
                                        [Element.text example.name
                                        , Element.text example.fileName]
                                )
                                (Nonempty.toList group.examples)
                            ]
                    )
                    (Nonempty.toList app.data.exampleGroups)
            ]
    }
