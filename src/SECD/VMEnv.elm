module SECD.VMEnv exposing (..)

import Element exposing (Element)
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Lib.Colours as Colours
import Lib.Cons as Cons exposing (Cons)
import Lib.Util as Util



-- "Environment" stack used by the VM when executing
-- Environment stack is essentially a list of lists, so it's a little more complicated than that
-- putting this in its own file for organization


type alias Environment a =
    List (EnvItem a)


type EnvItem a
    = ListItem (List (Cons a)) -- this ensures the Environment is at least nested 2 deep.
    | Dummy -- with doing recursion



-- debug


view : Int -> (a -> Element msg) -> Environment a -> Element msg
view n viewA env =
    List.map (viewEnvItem viewA) env
        |> Element.row
            [ Element.padding 8
            , Border.color Colours.purple
            , Border.width 1
            ]


viewEnvItem : (a -> Element msg) -> EnvItem a -> Element msg
viewEnvItem viewA item =
    case item of
        ListItem cs ->
            List.map (Cons.view viewA) cs
                |> List.intersperse (Element.text ",")
                |> Util.wrapAdd (Element.text "(") (Element.text ")")
                |> Element.row []

        Dummy ->
            Element.text "Dummy"


envItemToString : (a -> String) -> EnvItem a -> String
envItemToString aToString item =
    case item of
        ListItem c ->
            "[" ++ (String.join ", " <| List.map (Cons.toString aToString) c) ++ "]"

        Dummy ->
            "Dummy"



-- ops


init : Environment a
init =
    []


fromList : List (List a) -> Environment a
fromList =
    List.map (ListItem << List.map Cons.Val)


push : List (Cons a) -> Environment a -> Environment a
push xs env =
    ListItem xs :: env


pushDummy : Environment a -> Environment a
pushDummy env =
    Dummy :: env



-- locate is 0-indexed


locate : ( Int, Int ) -> Maybe (List (Cons a)) -> Environment a -> Result String (Cons a)
locate ( x, y ) mDummyVal environment =
    if (x < 0) || (y < 0) then
        Err "Negative index out of bounds"

    else
        case ( x, environment ) of
            ( _, [] ) ->
                Err "Locate: out of bounds!"

            ( 0, (ListItem h) :: _ ) ->
                locateRow y h

            ( 0, Dummy :: _ ) ->
                case mDummyVal of
                    Nothing ->
                        Err "Locate: attempt to access uninitialized Dummy Env value!"

                    Just dummyVal ->
                        locateRow y dummyVal

            ( _, _ :: t ) ->
                locate ( x - 1, y ) mDummyVal t


locateRow : Int -> List a -> Result String a
locateRow y row =
    case ( y, row ) of
        ( _, [] ) ->
            Err "Locate: out of bounds!"

        ( 0, h :: _ ) ->
            Ok h

        ( _, _ :: t ) ->
            locateRow (y - 1) t



-- replace the first row, IFF it is a dummy value


replaceDummy : List (Cons a) -> Environment a -> Result String (Environment a)
replaceDummy newRow env =
    case env of
        [] ->
            Err "replaceDummy: empty environment!"

        Dummy :: t ->
            Ok <| ListItem newRow :: t

        _ ->
            Err ""



---- DECODE ----


encode : (a -> Value) -> Environment a -> Value
encode subencoder env =
    Encode.list (encodeItem subencoder) env


encodeItem : (a -> Value) -> EnvItem a -> Value
encodeItem subencoder item =
    case item of
        ListItem cs ->
            Encode.object
                [ ( "t", Encode.string "I" )
                , ( "v", Encode.list (Cons.encode subencoder) cs )
                ]

        Dummy ->
            Encode.object [ ( "t", Encode.string "D" ) ]


decoder : Decoder a -> Decoder (Environment a)
decoder subdecoder =
    Decode.list (itemDecoder subdecoder)


itemDecoder : Decoder a -> Decoder (EnvItem a)
itemDecoder subdecoder =
    Decode.field "t" Decode.string
        |> Decode.andThen
            (\val ->
                case val of
                    "I" ->
                        Decode.field "v" (Decode.list (Cons.decoder subdecoder))
                            |> Decode.map ListItem

                    "D" ->
                        Decode.succeed Dummy

                    _ ->
                        Decode.fail <| "Expected ListItem or Dummy, got " ++ val
            )
