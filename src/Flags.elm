module Flags exposing (CodeExamples, Flags, Screen, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias Flags =
    { codeExamples : CodeExamples
    , screen : Screen
    }


decoder : Decoder Flags
decoder =
    Decode.map2 Flags
        (Decode.field "codeExamples" decodeCodeExamples)
        (Decode.field "screen" decodeScreen)


type alias CodeExamples =
    List ( String, List ( String, String ) )


decodeCodeExamples : Decoder CodeExamples
decodeCodeExamples =
    let
        decodeExample : Decoder (List ( String, String ))
        decodeExample =
            Decode.list <|
                Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.string)
    in
    Decode.map2 Tuple.pair (Decode.field "type" Decode.string) (Decode.field "examples" decodeExample)
        |> Decode.list


type alias Screen =
    { width : Int
    , height : Int
    }


decodeScreen : Decoder Screen
decodeScreen =
    Decode.map2 Screen
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
