module Flags exposing (CodeExamples, DefaultExample, Flags, Screen, decoder, findCodeExample)

import Html exposing (code)
import Json.Decode as Decode exposing (Decoder)


type alias Flags =
    { codeExamples : CodeExamples
    , defaultExample : DefaultExample
    }


type alias CodeExamples =
    List ( String, List ( String, String ) )


type alias Screen =
    { width : Int
    , height : Int
    }


type alias DefaultExample =
    { name : String
    , code : String
    , tab : String
    }


decoder : Decoder Flags
decoder =
    Decode.map2 Flags
        (Decode.field "codeExamples" decodeCodeExamples)
        (Decode.field "defaultExample" decodeDefaultExample)


decodeCodeExamples : Decoder CodeExamples
decodeCodeExamples =
    let
        decodeExample : Decoder (List ( String, String ))
        decodeExample =
            Decode.list <| decodeTuple Decode.string Decode.string
    in
    Decode.map2 Tuple.pair (Decode.field "type" Decode.string) (Decode.field "examples" decodeExample)
        |> Decode.list


decodeDefaultExample : Decoder DefaultExample
decodeDefaultExample =
    Decode.map3 DefaultExample
        (Decode.field "name" Decode.string)
        (Decode.field "code" Decode.string)
        (Decode.field "tab" Decode.string)



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



-- | Helpers
-- decode a JS two-element list into a tuple


decodeTuple : Decoder a -> Decoder b -> Decoder ( a, b )
decodeTuple a b =
    Decode.map2 Tuple.pair (Decode.index 0 a) (Decode.index 1 b)
