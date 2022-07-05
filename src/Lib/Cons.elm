module Lib.Cons exposing (..)

import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Cons a
    = Nil -- also acts as the Nil pointer in the SECD VM
    | Val a
    | Cons (Cons a) (Cons a) -- can be deeply nested cons



-- constructors


nil : Cons a
nil =
    Nil


single : a -> Cons a
single =
    Val


cons : Cons a -> Cons a -> Cons a
cons =
    Cons


fromList : List a -> Cons a
fromList l =
    case l of
        [] ->
            Nil

        x :: xs ->
            Cons (Val x) (fromList xs)



-- deconstructors
-- can only return a list if the Cons ends in a NIL


toList : Cons a -> Maybe (List (Cons a))
toList c =
    case c of
        Nil ->
            Just []

        Cons a xs ->
            Maybe.map ((::) a) (toList xs)

        _ ->
            Nothing



-- usually for testing


fromConsList : List (Cons a) -> Cons a
fromConsList l =
    case l of
        [] ->
            Nil

        x :: xs ->
            Cons x (fromConsList xs)



-- displays the cons array similar to how Lisp does it


toString : (a -> String) -> Cons a -> String
toString aToString c =
    case c of
        Nil ->
            "Nil"

        Val a ->
            aToString a

        ct ->
            "(" ++ toStringHelper aToString ct ++ ")"


toStringHelper : (a -> String) -> Cons a -> String
toStringHelper aToString c =
    case c of
        Nil ->
            "Nil"

        Val a ->
            aToString a

        -- end of list
        Cons a Nil ->
            toString aToString a

        -- add a dot
        Cons ca (Val a) ->
            toString aToString ca ++ " . " ++ aToString a

        -- run toStringHelper on the remaining, because we don't need parentheses
        Cons ca rest ->
            toString aToString ca ++ " " ++ toStringHelper aToString rest



-- toString but instead, it returns it as an HTML element


view : (a -> Element msg) -> Cons a -> Element msg
view viewA c =
    case c of
        Nil ->
            Element.text "Nil"

        Val a ->
            viewA a

        ct ->
            Element.row
                [ Element.width Element.fill ]
                [ Element.text "("
                , viewHelper viewA ct
                , Element.text ")"
                ]


viewHelper : (a -> Element msg) -> Cons a -> Element msg
viewHelper viewA c =
    case c of
        Nil ->
            Element.text "Nil"

        Val a ->
            viewA a

        -- end of list
        Cons a Nil ->
            view viewA a

        -- add a dot
        Cons ca (Val a) ->
            Element.row
                [ Element.width Element.fill, Element.spacing 4 ]
                [ view viewA ca
                , Element.text "."
                , viewA a
                ]

        -- run toStringHelper on the remaining, because we don't need parentheses
        Cons ca rest ->
            Element.row
                [ Element.width Element.fill, Element.spacing 4 ]
                [ view viewA ca
                , Element.text " "
                , viewHelper viewA rest
                ]



---- DECODER ----


encode : (a -> Value) -> Cons a -> Value
encode subEncoder c =
    case c of
        Nil ->
            Encode.object [ ( "t", Encode.string "n" ) ]

        Val a ->
            Encode.object [ ( "t", Encode.string "v" ), ( "v", subEncoder a ) ]

        Cons hd tl ->
            Encode.object [ ( "t", Encode.string "c" ), ( "hd", encode subEncoder hd ), ( "tl", encode subEncoder tl ) ]


decoder : Decoder a -> Decoder (Cons a)
decoder subdecoder =
    Decode.field "t" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "n" ->
                        Decode.succeed Nil

                    "v" ->
                        Decode.map Val (Decode.field "v" subdecoder)

                    "c" ->
                        Decode.map2 Cons (Decode.field "hd" (decoder subdecoder)) (Decode.field "tl" (decoder subdecoder))

                    _ ->
                        Decode.fail <| "Unknown tag: " ++ tag
            )
