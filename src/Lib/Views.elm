module Lib.Views exposing (..)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Lib.Colours as Colours



-- composable views


button : msg -> Element msg -> Element msg
button msg label =
    Input.button
        [ Element.paddingXY 12 8
        , Border.color <| Element.rgb 0 0 0
        , Border.width 1
        , Border.rounded 5
        , Element.centerX
        , Element.mouseOver
            [ Background.color Colours.slateGrey ]
        ]
        { onPress = Just msg
        , label = label
        }



-- inline code block


link :
    List (Attribute msg)
    ->
        { url : String
        , label : String
        }
    -> Element msg
link attrs { url, label } =
    Element.link
        ([ Font.color Colours.linkBlue
         , Element.mouseDown [ Font.color Colours.linkIndigo ]
         , Element.mouseOver [ Font.color Colours.linkLightBlue ]
         ]
            ++ attrs
        )
        { url = url
        , label = Element.text label
        }


unselectable : Attribute msg
unselectable =
    Element.htmlAttribute <| Html.Attributes.class "unselectable"



-- Paragraph helpers


bold : String -> Element msg
bold =
    Element.el [ Font.bold ] << Element.text
