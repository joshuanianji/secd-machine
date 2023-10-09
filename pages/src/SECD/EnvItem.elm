module SECD.EnvItem exposing (..)

import Element exposing (Element)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Lib.Cons as Cons exposing (Cons)
import Lib.Util as Util



-- Environment stack is essentially a list of lists, so it's a little more complicated than that
-- since we also have to include a dummy value


type EnvItem a
    = ListItem (List (Cons a)) -- this ensures the Environment is at least nested 2 deep.
    | Dummy -- with doing recursion



-- debug


view : (a -> Element msg) -> EnvItem a -> Element msg
view viewA item =
    case item of
        ListItem cs ->
            List.map (Cons.view viewA) cs
                |> List.intersperse (Element.text " ")
                |> Util.wrapAdd (Element.text "(") (Element.text ")")
                |> Element.row []

        Dummy ->
            Element.text "Ω"



---- DECODE ----


encode : (a -> Value) -> EnvItem a -> Value
encode subencoder item =
    case item of
        ListItem cs ->
            Encode.object
                [ ( "t", Encode.string "I" )
                , ( "v", Encode.list (Cons.encode subencoder) cs )
                ]

        Dummy ->
            Encode.object [ ( "t", Encode.string "D" ) ]


decoder : Decoder a -> Decoder (EnvItem a)
decoder subdecoder =
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
