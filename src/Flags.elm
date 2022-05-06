module Flags exposing (CodeExamples, Flags, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias Flags =
    { codeExamples : CodeExamples }


decoder : Decoder Flags
decoder =
    Decode.map Flags
        (Decode.field "codeExamples" decodeCodeExamples)


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
