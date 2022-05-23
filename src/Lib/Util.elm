module Lib.Util exposing (..)

import Element exposing (Attribute, Element)
import FeatherIcons exposing (Icon)
import List.Zipper as Zipper exposing (Zipper)
import Parser exposing (DeadEnd, Problem(..))


viewIcon : List (Attribute msg) -> Icon -> Element msg
viewIcon attrs =
    FeatherIcons.toHtml [] >> Element.html >> Element.el attrs


eachZero : { top : Int, right : Int, bottom : Int, left : Int }
eachZero =
    { top = 0, right = 0, bottom = 0, left = 0 }


eachZeroBorder : { topLeft : Int, topRight : Int, bottomRight : Int, bottomLeft : Int }
eachZeroBorder =
    { topLeft = 0, topRight = 0, bottomRight = 0, bottomLeft = 0 }


wrapAdd : a -> a -> List a -> List a
wrapAdd first last l =
    first :: l ++ [ last ]


showMaybeInt : Maybe Int -> String
showMaybeInt mn =
    case mn of
        Just n ->
            String.fromInt n

        Nothing ->
            "unknown"


foldResult : Result a a -> a
foldResult r =
    case r of
        Ok a ->
            a

        Err a ->
            a


{-|


## focusN

focus on the nth element in a zipper

If `n<=0`, we focus on the first element

If `n` is out of bounds, the zipper points to the last element

-}
zipperNth : Int -> Zipper a -> Zipper a
zipperNth n z =
    let
        first =
            Zipper.first z

        helper n_ z_ =
            if n_ <= 0 then
                z_

            else
                case Zipper.next z_ of
                    Nothing ->
                        z_

                    Just z__ ->
                        helper (n_ - 1) z__
    in
    helper n first


{-|


## getLocationInfo

takes in an index and finds out which page and chunk it's from. 0-indexed.

Returns:

    { pageNum = Int -- which page to retrieve JS from
    , pageLocation = Int -- where inside the page to load the chunk
    , chunkLocation = Int -- where inside the chunk to go to
    }

-}
getLocationInfo : Int -> { a | pageSize : Int, chunkSize : Int } -> { pageNum : Int, pageLocation : Int, chunkLocation : Int }
getLocationInfo n { pageSize, chunkSize } =
    { pageNum = n // (pageSize * chunkSize)
    , pageLocation = modBy (pageSize * chunkSize) n // chunkSize
    , chunkLocation = modBy chunkSize n
    }



-- view utils


surround : List (Attribute msg) -> { left : Int, middle : Int, right : Int } -> Element msg -> Element msg
surround attrs { left, middle, right } =
    \el ->
        Element.row
            (Element.width Element.fill :: attrs)
            [ Element.el [ Element.width <| Element.fillPortion left, Element.height Element.fill ] Element.none
            , Element.el [ Element.width <| Element.fillPortion middle, Element.height Element.fill ] el
            , Element.el [ Element.width <| Element.fillPortion right, Element.height Element.fill ] Element.none
            ]



-- taken from an Elm-compiler PR


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"
