module Lib.Cons exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr


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


view : (a -> Html msg) -> Cons a -> Html msg
view viewA c =
    case c of
        Nil ->
            Html.div [ Attr.class "vm-cons cons-nil" ] [ Html.text "Nil" ]

        Val a ->
            Html.div [ Attr.class "vm-cons cons-val" ] [ viewA a ]

        ct ->
            Html.div
                [ Attr.class "vm-cons row cons-cons" ]
                [ Html.text "("
                , viewHelper viewA ct
                , Html.text ")"
                ]


viewHelper : (a -> Html msg) -> Cons a -> Html msg
viewHelper viewA c =
    case c of
        Nil ->
            Html.div [ Attr.class "vm-cons cons-nil" ] [ Html.text "Nil" ]

        Val a ->
            Html.div [ Attr.class "vm-cons cons-val" ] [ viewA a ]

        -- end of list
        Cons a Nil ->
            view viewA a

        -- add a dot
        Cons ca (Val a) ->
            Html.div
                [ Attr.class "vm-cons row cons-val" ]
                [ view viewA ca
                , Html.text "."
                , viewA a
                ]

        -- run toStringHelper on the remaining, because we don't need parentheses
        Cons ca rest ->
            Html.div
                [ Attr.class "vm-cons row cons-val" ]
                [ view viewA ca
                , Html.text " "
                , viewHelper viewA rest
                ]
