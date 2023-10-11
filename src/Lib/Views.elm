module Lib.Views exposing (..)

import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Lib.Colours as Colours
import Lib.Util as Util



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


toggleButtons : Bool -> List (Attribute msg) -> List { active : Bool, onPress : Maybe msg, label : Element msg } -> Element msg
toggleButtons enabled inputAttrs btnOpts =
    let
        ( onPress, parentColour, attrs ) =
            if not enabled then
                -- disable on rowview
                ( \_ -> Nothing
                , Colours.greyAlpha 0.3
                , \_ ->
                    [ Element.padding 8
                    , Font.color <| Colours.greyAlpha 0.3
                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
                    ]
                )

            else
                ( identity
                , Colours.black
                , enabledAttrs
                )

        enabledAttrs : Bool -> List (Element.Attribute msg)
        enabledAttrs active =
            [ Element.padding 8 ]
                |> Util.addIf active [ Background.color Colours.black, Font.color Colours.white ]
                |> Util.addIf (not active) [ Element.mouseOver [ Background.color Colours.slateGrey ] ]
    in
    List.map
        (\btn ->
            Input.button
                (attrs btn.active)
                { onPress = onPress btn.onPress
                , label = btn.label
                }
        )
        btnOpts
        |> Element.row
            ([ Border.rounded 8
             , Border.color parentColour
             , Border.width 1

             -- hides children's background by this element's border radius
             , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
             , Element.htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
             ]
                ++ inputAttrs
            )


unselectable : Attribute msg
unselectable =
    Element.htmlAttribute <| Html.Attributes.class "unselectable"



-- a title for a block of content that conditionally displays


togglableTitle : List (Attribute msg) -> { label : String, activeWhen : Bool, onClick : msg } -> Element msg
togglableTitle attrs { label, activeWhen, onClick } =
    let
        ( fontColor, icon ) =
            if activeWhen then
                ( Colours.grey, FeatherIcons.chevronDown )

            else
                ( Colours.greyAlpha 0.5, FeatherIcons.chevronRight )
    in
    Element.el
        ([ Font.size 32
         , Font.color fontColor
         , Font.bold
         , Events.onClick onClick
         , unselectable
         , Element.pointer
         , Element.mouseOver
            [ Font.color <| Colours.lightGrey ]
         , Element.onLeft (Util.viewIcon [ Element.scale 1.25, Font.bold, Element.centerY, Element.paddingXY 8 0 ] icon)
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
