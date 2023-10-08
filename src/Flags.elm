module Flags exposing (CodeExamples, Flags, Screen, decoder, findCodeExample)

import Json.Decode as Decode exposing (Decoder)


type alias Flags =
    { codeExamples : CodeExamples
    , screen : Screen
    , defaultExample : ( String, String )
    }


decoder : Decoder Flags
decoder =
    Decode.map3 Flags
        (Decode.field "codeExamples" decodeCodeExamples)
        (Decode.field "screen" decodeScreen)
        (Decode.field "defaultExample" <| decodeTuple Decode.string Decode.string)


type alias CodeExamples =
    List ( String, List ( String, String ) )


decodeCodeExamples : Decoder CodeExamples
decodeCodeExamples =
    let
        decodeExample : Decoder (List ( String, String ))
        decodeExample =
            Decode.list <| decodeTuple Decode.string Decode.string
    in
    Decode.map2 Tuple.pair (Decode.field "type" Decode.string) (Decode.field "examples" decodeExample)
        |> Decode.list



-- given a name, find the example code and tab


findCodeExample : String -> CodeExamples -> Maybe ( String, String )
findCodeExample name examples =
    let
        search : CodeExamples -> Maybe ( String, String )
        search exs =
            case exs of
                [] ->
                    Nothing

                ( tab, exsGroup ) :: rest ->
                    case searchTab exsGroup of
                        Nothing ->
                            search rest

                        Just code ->
                            Just ( tab, code )

        searchTab : List ( String, String ) -> Maybe String
        searchTab exsGroup =
            case exsGroup of
                [] ->
                    Nothing

                ( exName, code ) :: rest ->
                    if name == exName then
                        Just code

                    else
                        searchTab rest
    in
    search examples


type alias Screen =
    { width : Int
    , height : Int
    }


decodeScreen : Decoder Screen
decodeScreen =
    Decode.map2 Screen
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)



-- | Helpers
-- decode a JS two-element list into a tuple


decodeTuple : Decoder a -> Decoder b -> Decoder ( a, b )
decodeTuple a b =
    Decode.map2 Tuple.pair (Decode.index 0 a) (Decode.index 1 b)
