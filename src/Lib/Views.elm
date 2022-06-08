module Lib.Views exposing (..)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
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



-- a title for a block of content that conditionally displays


togglableTitle : List (Attribute msg) -> { label : String, activeWhen : Bool, onClick : msg } -> Element msg
togglableTitle attrs { label, activeWhen, onClick } =
    Element.el
        ([ Font.size 32
         , Font.color <|
            if activeWhen then
                Colours.grey

            else
                Colours.greyAlpha 0.5
         , Font.bold
         , Events.onClick onClick
         , unselectable
         , Element.pointer
         , Element.mouseOver
            [ Font.color <| Colours.lightGrey ]
         ]
            ++ attrs
        )
        (Element.text label)


viewTogglable : List (Attribute msg) -> { title : String, activeWhen : Bool, onClick : msg, body : Element msg } -> Element msg
viewTogglable attrs { title, activeWhen, onClick, body } =
    Element.column
        ([ Element.spacing 32
         , Element.width Element.fill
         ]
            ++ attrs
        )
        [ togglableTitle
            []
            { label = title
            , activeWhen = activeWhen
            , onClick = onClick
            }
        , if activeWhen then
            body

          else
            Element.none
        ]



-- Paragraph helpers


bold : String -> Element msg
bold =
    Element.el [ Font.bold ] << Element.text
