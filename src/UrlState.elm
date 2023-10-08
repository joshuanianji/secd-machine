module UrlState exposing (UrlState, fromUrl, merge, navigateTo)

import Browser.Navigation as Nav
import Dict
import Url exposing (Url)
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query



-- | A dirt-simple strategy for encoding and decoding global state
-- | Right now, we just have the tab we are on


type alias UrlState =
    { -- which "example" tab we are on
      tab : String
    }


default : UrlState
default =
    { tab = "Arithmetic" }



-- PUBLIC HELPERS


navigateTo : Nav.Key -> UrlState -> Cmd msg
navigateTo key route =
    Nav.pushUrl key (toUrlString route)


fromUrl : Url -> UrlState
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Url.Parser.parse parser
        |> Maybe.withDefault default



-- once we have a new route, we need to merge it with the current route
-- this is supposed to help me in the future *if* the url state gets more complicated


merge : UrlState -> UrlState -> UrlState
merge newRoute currentRoute =
    { currentRoute | tab = newRoute.tab }



-- Parsers


parser : Parser (UrlState -> a) a
parser =
    Url.Parser.map UrlState queryParsers


queryParsers : Parser (String -> a) a
queryParsers =
    Url.Parser.top
        <?> (Query.map (Maybe.withDefault "Arithmetic") <| Query.string "tab")


toUrlString : UrlState -> String
toUrlString route =
    "?tab=" ++ route.tab
