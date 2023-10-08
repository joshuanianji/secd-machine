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

type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { exampleGroups : List Backend.GetExamplesTask.ExampleGroup
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
                                group.examples
                            ]
                    )
                    app.data.exampleGroups
            ]
    }
