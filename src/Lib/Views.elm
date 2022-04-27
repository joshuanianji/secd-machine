module Lib.Views exposing (..)

import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input



-- composable views


button : msg -> Element msg -> Element msg
button msg label =
    Input.button
        [ Element.paddingXY 12 8
        , Border.color <| Element.rgb 0 0 0
        , Border.width 1
        , Border.rounded 5
        , Element.centerX
        ]
        { onPress = Just msg
        , label = label
        }
