module UrlState exposing (UrlState, default, fromUrl, merge, navigateTo, updateTab)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query



-- | A dirt-simple strategy for encoding and decoding global state (across page refreshes)
-- | Right now, we just keep track of the example we are on


type alias UrlState =
    { -- which "example" tab we are on
      exampleName : String
    }


default : UrlState
default =
    { exampleName = "Arithmetic" }



-- PUBLIC HELPERS


updateTab : String -> Nav.Key -> UrlState -> ( UrlState, Cmd msg )
updateTab tab key state =
    let
        newState =
            { state | exampleName = tab }
    in
    ( newState, navigateTo key newState )


fromUrl : Url -> UrlState
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Url.Parser.parse parser
        |> Maybe.withDefault default



-- once we have a new route, we need to merge it with the current route
-- this is supposed to help me in the future *if* the url state gets more complicated


merge : UrlState -> UrlState -> UrlState
merge newState currentState =
    { currentState | exampleName = newState.exampleName }



-- Parsers


parser : Parser (UrlState -> a) a
parser =
    Url.Parser.map UrlState queryParsers


queryParsers : Parser (String -> a) a
queryParsers =
    Url.Parser.top
        <?> (Query.map (Maybe.withDefault "Arithmetic") <| Query.string "tab")


toUrlString : UrlState -> String
toUrlString state =
    Url.Builder.relative [] [ Url.Builder.string "tab" state.exampleName ]



-- Private helper


navigateTo : Nav.Key -> UrlState -> Cmd msg
navigateTo key route =
    Nav.pushUrl key (toUrlString route)
