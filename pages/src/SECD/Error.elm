module SECD.Error exposing (..)

import Html exposing (Html)


type alias Error =
    String


view : Error -> Html msg
view =
    Html.text
