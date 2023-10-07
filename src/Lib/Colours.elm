module Lib.Colours exposing (..)

import Element exposing (Color)


{-| The higher the alpha, the less transparent the colour is.
-}
greyAlpha : Float -> Color
greyAlpha =
    Element.rgba255 38 50 56


slateGrey : Color
slateGrey =
    greyAlpha 0.05


lightGrey : Color
lightGrey =
    greyAlpha 0.8


grey : Color
grey =
    greyAlpha 1


transparent : Color
transparent =
    Element.rgba 0 0 0 0



-- colours taken from code


purple : Color
purple =
    Element.rgb255 199 146 234


orange : Color
orange =
    Element.rgb255 247 140 108


red : Color
red =
    Element.rgb255 255 83 112


darkgrey : Color
darkgrey =
    Element.rgb255 84 110 122



-- link colours, taken from Wikipedia
-- https://en.wikipedia.org/wiki/Help:Link_color


linkBlue : Color
linkBlue =
    Element.rgb255 6 69 173


linkLightBlue : Color
linkLightBlue =
    Element.rgb255 51 102 187


linkIndigo : Color
linkIndigo =
    Element.rgb255 11 0 128


black : Color
black =
    Element.rgb255 0 0 0


white : Color
white =
    Element.rgb255 255 255 255
