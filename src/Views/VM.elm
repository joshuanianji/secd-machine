module Views.VM exposing (Model, Msg, init, subscriptions, update, view)

-- | Vm View
-- Views the VM, and lets the user go back and forth from different VM states

import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Lib.Colours as Colours
import Lib.Util as Util exposing (eachZero)
import Lib.Views
import List.Zipper as Zipper exposing (Zipper)
import Ordinal exposing (ordinal)
import Ports
import SECD.Error as Error exposing (Error)
import SECD.Program exposing (Program)
import SECD.VM as VM exposing (Align(..), VM, VMResult)



---- MODEL ----


type alias Model =
    { -- which state index are we in?
      -- also, keep track of the slider position separately
      index : Int
    , stateSliderIdx : Int

    -- current chunk holds the VM states in the current chunk
    , chunk : Zipper VM

    -- current page holds the starting positions for each chunk
    , page : Zipper VM

    -- Keeps track of the IDs of all the generated pages, if we need to swap a page
    , pages : Zipper Int

    -- furthest calculated VM state (either a state, or a Result)
    , latestVM : Result VM VMResult

    -- total states to keep track of
    , totalStates : Int
    , pressedKeys : List Key
    , pagesInfo : PagesInfo
    , options : Options

    -- keeping compiled code to view
    , compiled : Program

    -- status when we're fetching a page
    , fetchStatus : FetchStatus
    }



-- OPTIONS


type alias Options =
    { viewDepth : Int
    , rowView : Bool

    -- this only affects column views
    , align : Align
    }


initOptions : Options
initOptions =
    { viewDepth = 10
    , rowView = False
    , align = Left
    }


type FetchStatus
    = Idle
    | Loading { pageLocation : Location, chunkLocation : Location }
    | Error Error



-- Lets us know where in the page or chunk we want to go.


type Location
    = Beginning
    | End
    | Indexed Int -- specific index (0-indexed)


type alias PagesInfo =
    { maxPages : Int, pageSize : Int, chunkSize : Int }


init : PagesInfo -> Program -> ( Model, Cmd Msg )
init pagesInfo prog =
    let
        vm =
            VM.initRaw prog

        pagesData =
            VM.getPages pagesInfo vm
    in
    ( { index = 0
      , stateSliderIdx = 0
      , chunk = pagesData.initialChunk
      , page = pagesData.initialPage
      , pages = pagesData.pages
      , latestVM = pagesData.result
      , totalStates = pagesData.totalVMCount
      , pressedKeys = []
      , pagesInfo = pagesInfo
      , options = initOptions
      , compiled = prog
      , fetchStatus = Idle
      }
    , Ports.sendPages pagesData.toJSValues
    )



---- MSG ----


type Msg
    = First
    | Previous
    | Step
    | Last
    | ToIndex Int
    | UpdateStateSlider Int
    | UpdateOptions OptionsMsg
    | KeyMsg Keyboard.Msg
    | GotPage (Result Error ( Int, Zipper VM ))
    | Blur
    | NoOp


type OptionsMsg
    = UpdateDepth Int
    | UpdateRowView Bool
    | UpdateAlign Align



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.fetchStatus, msg ) of
        ( _, First ) ->
            if Zipper.isFirst model.pages then
                ( model
                    |> updatePageLocation
                        { pageLocation = Just Beginning
                        , chunkLocation = Beginning
                        }
                    |> updateStateIndex 0
                , Cmd.none
                )

            else
                ( { model
                    | pages = Zipper.first model.pages
                    , fetchStatus = Loading { pageLocation = Beginning, chunkLocation = Beginning }
                  }
                    |> updateStateIndex 0
                , Ports.fetchPage 0
                )

        ( _, Previous ) ->
            case Zipper.previous model.chunk of
                Just newState ->
                    ( { model | chunk = newState }
                        |> updateStateIndex (model.index - 1)
                    , Cmd.none
                    )

                Nothing ->
                    -- get previous chunk
                    case Zipper.previous model.page of
                        Just newPage ->
                            ( { model | page = newPage }
                                |> updatePageLocation
                                    { pageLocation = Nothing
                                    , chunkLocation = End
                                    }
                                |> updateStateIndex (model.index - 1)
                            , Ports.log "Previous: New chunk!"
                            )

                        Nothing ->
                            case Zipper.previous model.pages of
                                Just newPages ->
                                    -- fetch previous page from JS
                                    ( { model
                                        | pages = newPages
                                        , fetchStatus = Loading { pageLocation = End, chunkLocation = End }
                                      }
                                        |> updateStateIndex (model.index - 1)
                                    , Ports.fetchPage <| Zipper.current newPages
                                    )

                                Nothing ->
                                    -- We are at the beginning of the program
                                    ( model, Cmd.none )

        ( _, Step ) ->
            case Zipper.next model.chunk of
                Just newState ->
                    ( { model | chunk = newState }
                        |> updateStateIndex (model.index + 1)
                    , Cmd.none
                    )

                Nothing ->
                    -- get next chunk
                    case Zipper.next model.page of
                        Just newPage ->
                            ( { model | page = newPage }
                                |> updatePageLocation
                                    { pageLocation = Nothing
                                    , chunkLocation = Beginning
                                    }
                                |> updateStateIndex (model.index + 1)
                            , Ports.log "Step: New chunk!"
                            )

                        Nothing ->
                            case Zipper.next model.pages of
                                Just newPages ->
                                    -- fetch previous page from JS
                                    ( { model
                                        | pages = newPages
                                        , fetchStatus = Loading { pageLocation = Beginning, chunkLocation = Beginning }
                                      }
                                        |> updateStateIndex (model.index + 1)
                                    , Ports.fetchPage <| Zipper.current newPages
                                    )

                                Nothing ->
                                    -- We are at the end of the program
                                    ( model, Cmd.none )

        ( _, Last ) ->
            if Zipper.isLast model.pages then
                ( model
                    |> updatePageLocation
                        { pageLocation = Just End
                        , chunkLocation = End
                        }
                    |> updateStateIndex (model.totalStates - 1)
                , Cmd.none
                )

            else
                -- swap page
                ( { model
                    | pages = Zipper.last model.pages
                    , fetchStatus = Loading { pageLocation = End, chunkLocation = End }
                  }
                    |> updateStateIndex (model.totalStates - 1)
                , Ports.fetchPage (Zipper.current <| Zipper.last model.pages)
                )

        ( _, ToIndex n ) ->
            let
                { pageNum, pageLocation, chunkLocation } =
                    Util.getLocationInfo n model.pagesInfo
            in
            if Zipper.current model.pages == pageNum then
                ( model
                    |> updatePageLocation
                        { pageLocation = Just <| Indexed pageLocation
                        , chunkLocation = Indexed chunkLocation
                        }
                    |> updateStateIndex n
                , Cmd.none
                )

            else
                ( { model
                    | pages = Util.zipperNth pageNum model.pages
                    , fetchStatus = Loading { pageLocation = Indexed pageLocation, chunkLocation = Indexed chunkLocation }
                  }
                    |> updateStateIndex n
                , Ports.fetchPage pageNum
                )

        ( _, UpdateStateSlider val ) ->
            ( { model | stateSliderIdx = val }, Cmd.none )

        ( _, UpdateOptions optionsMsg ) ->
            ( { model | options = updateOptions optionsMsg model.options }, Cmd.none )

        ( _, KeyMsg keyMsg ) ->
            let
                newPressedKeys =
                    Keyboard.update keyMsg model.pressedKeys

                newModel =
                    { model | pressedKeys = newPressedKeys }

                arrows =
                    Keyboard.Arrows.arrows newPressedKeys
            in
            if arrows.x == 1 then
                -- right key
                update Step newModel

            else if arrows.x == -1 then
                update Previous newModel

            else
                ( newModel, Cmd.none )

        ( Loading locs, GotPage res ) ->
            case res of
                Ok ( _, page ) ->
                    ( { model | fetchStatus = Idle, page = page }
                        |> updatePageLocation
                            { pageLocation = Just locs.pageLocation
                            , chunkLocation = locs.chunkLocation
                            }
                    , Cmd.none
                    )

                Err n ->
                    ( { model | fetchStatus = Error n }, Cmd.none )

        ( _, Blur ) ->
            ( { model | pressedKeys = [] }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


updateOptions : OptionsMsg -> Options -> Options
updateOptions msg options =
    case msg of
        UpdateDepth n ->
            { options | viewDepth = n }

        UpdateRowView b ->
            { options | rowView = b }

        UpdateAlign newAlign ->
            { options | align = newAlign }


{-|


## updatePageLocation

`update` helper function

Updates current page (zipper of Chunk VM inits), and calculates a new chunk for it. Does not move to a new page.

**NOTE**: If the Location is `Indexed`, we take in 0-indexed values.

If pageLocation is `Nothing`, do not update the page zipper and just calculate a new chunk.

Remember:

  - Page: a list of states that represent the beginning of each chunk
  - Chunk: a group of sequential states

Returns: (`Page Zipper`, `Chunk Zipper`)

-}
updatePageLocation : { pageLocation : Maybe Location, chunkLocation : Location } -> Model -> Model
updatePageLocation { pageLocation, chunkLocation } model =
    let
        locToZipper : Maybe Location -> Zipper a -> Zipper a
        locToZipper loc =
            case loc of
                Just Beginning ->
                    Zipper.first

                Just End ->
                    Zipper.last

                Just (Indexed idx) ->
                    Util.zipperNth idx

                Nothing ->
                    identity

        -- update zipper pointer in the page
        newPage =
            locToZipper pageLocation model.page

        ( remainingChunk, _ ) =
            VM.stepNAccumulate (model.pagesInfo.chunkSize - 1) (Zipper.current newPage)

        -- update chunk pointer in the chunk (single evaledChunk will always point to the first element)
        newChunk =
            Zipper.fromCons (Zipper.current newPage) remainingChunk
                |> locToZipper (Just chunkLocation)
    in
    { model | page = newPage, chunk = newChunk }


{-|


## updateStateIndex

updates `index` and `stateSliderIdx` fields of the model.

-}
updateStateIndex : Int -> Model -> Model
updateStateIndex newIdx model =
    { model | index = newIdx, stateSliderIdx = newIdx }



---- VIEW ----


view : Model -> Element Msg
view model =
    let
        title =
            Element.el
                [ Font.size 32
                , Font.color Colours.grey
                , Font.bold
                , Element.paddingEach { eachZero | bottom = 16 }
                ]
                (Element.text "Execute Program on VM")

        totalSize =
            Element.paragraph
                []
                [ Element.text "Total size: "
                , Lib.Views.bold <| String.fromInt model.totalStates
                , case model.latestVM of
                    Err _ ->
                        Lib.Views.bold "+"

                    Ok _ ->
                        Element.none
                , Element.text " states"
                ]

        n =
            if model.options.viewDepth == 21 then
                Nothing

            else
                Just model.options.viewDepth
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 16
        , Element.spacingXY 8 12
        ]
        [ title
        , totalSize

        -- this slider lets us jump states
        , viewStateSlider model

        -- renders if the fetch status is error
        , viewFetchError model
        , case model.latestVM of
            Err _ ->
                Element.text "Warning: Latest VM does not terminate!"

            Ok _ ->
                Element.none
        , viewOptions model.options

        -- stuff to render when we're at the final VM state
        , finalVMState model
        , Zipper.current model.chunk
            |> VM.view
                { depth = n
                , aligns = model.options.align
                , rowView = model.options.rowView
                }
            |> Element.el
                [ Element.width Element.fill
                , Element.paddingXY 8 12
                , Border.rounded 8
                , Border.width 1
                , Border.color <| Colours.greyAlpha 0.2
                , Element.scrollbarX

                -- without this (while we have scrollbarX), the height will be 0
                , Element.htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
                ]
        ]


viewStateSlider : Model -> Element Msg
viewStateSlider model =
    let
        sliderBtn msg icon isDisabled tooltip =
            Input.button
                ([ Element.padding 8
                 , Font.size 20
                 , Border.rounded 8
                 , Element.centerX
                 , Element.htmlAttribute <| Html.Attributes.attribute "title" tooltip
                 ]
                    |> Util.addIf isDisabled
                        [ Font.color <| Colours.greyAlpha 0.5
                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
                        ]
                    |> Util.addIf (not isDisabled)
                        [ Element.mouseOver [ Background.color Colours.slateGrey ] ]
                )
                { onPress = Just msg
                , label = Util.viewIcon [ Font.size 16 ] icon
                }
    in
    Element.column
        [ Element.width Element.fill
        , Element.paddingXY 8 12
        , Element.spacing 8
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 6
            ]
            [ sliderBtn First FeatherIcons.chevronsLeft (atFirstState model) "Jump to the first state"
            , sliderBtn Previous FeatherIcons.chevronLeft (atFirstState model) "Go to the previous state (←)"
            , Input.slider
                [ Element.width Element.fill

                -- same height as the thumb
                , Element.height (Element.px 24)

                -- establish the "track"
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 8)
                        , Element.centerY
                        , Background.color <| Colours.greyAlpha 0.1
                        , Border.rounded 4
                        ]
                        Element.none
                    )
                ]
                { onChange = round >> UpdateStateSlider
                , label = Input.labelHidden "Slider for VM State Index"
                , min = 0
                , max = toFloat <| model.totalStates - 1
                , value = toFloat model.stateSliderIdx
                , thumb =
                    Input.thumb
                        [ Element.width (Element.px 32)
                        , Element.height (Element.px 32)
                        , Border.rounded 32
                        , Background.color <| Colours.purple
                        , Element.inFront
                            (Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Font.size 16
                                , Font.color Colours.white
                                ]
                                (Element.text <| String.fromInt (model.index + 1))
                            )
                        ]
                , step = Just 1
                }
            , sliderBtn Step FeatherIcons.chevronRight (atLastState model) "Go to the next state (→)"
            , sliderBtn Last FeatherIcons.chevronsRight (atLastState model) "Jump to the last state"
            ]
        , if model.stateSliderIdx /= model.index then
            Lib.Views.button (ToIndex model.stateSliderIdx) <| Element.text ("Go to " ++ ordinal (model.stateSliderIdx + 1) ++ " state")

          else
            Element.el [ Element.height (Element.px 38) ] Element.none
        ]


viewFetchError : Model -> Element Msg
viewFetchError model =
    case model.fetchStatus of
        Error n ->
            Element.paragraph
                []
                [ Element.text "Error: "
                , Element.text n
                ]

        _ ->
            Element.none



-- | renders the options menu


viewOptions : Options -> Element Msg
viewOptions options =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 24
        ]
        [ vmViewSlider options
        , rowToggle options
        , alignToggles options
        ]
        |> Util.surround [] { left = 1, middle = 3, right = 1 }



-- this slider determines how much of the VM we show


vmViewSlider : Options -> Element Msg
vmViewSlider options =
    let
        labelText =
            if options.viewDepth == 21 then
                Element.text "Showing entire VM"

            else
                Element.text ("Peeking " ++ String.fromInt options.viewDepth ++ " value(s) into the VM")
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 8
        ]
        [ Element.el [ Element.alignBottom, Element.padding 2 ] <| Element.text "1"
        , Input.slider
            [ -- same height as the thumb
              Element.height (Element.px 24)

            -- establish the "track"
            , Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 8)
                    , Element.centerY
                    , Background.color <| Colours.greyAlpha 0.1
                    , Border.rounded 4
                    ]
                    Element.none
                )
            ]
            { onChange = round >> UpdateDepth >> UpdateOptions
            , label = Input.labelAbove [ Font.center ] labelText
            , min = 1
            , max = 21
            , value = toFloat options.viewDepth
            , thumb =
                Input.thumb
                    [ Element.width (Element.px 24)
                    , Element.height (Element.px 24)
                    , Border.rounded 24
                    , Background.color <| Colours.purple
                    ]
            , step = Just 1
            }
        , Element.el [ Element.alignBottom, Element.padding 2 ] <| Element.text "∞"
        ]



-- toggles between rows and column views


rowToggle : Options -> Element Msg
rowToggle options =
    let
        onPress rowView =
            Just (UpdateOptions <| UpdateRowView rowView)
    in
    Lib.Views.toggleButtons True
        []
        [ { active = options.rowView
          , onPress = onPress True
          , label = Element.text "Row View"
          }
        , { active = not options.rowView
          , onPress = onPress False
          , label = Element.text "Column View"
          }
        ]



-- toggles between the three aligns


alignToggles : Options -> Element Msg
alignToggles options =
    let
        onPress align =
            Just (UpdateOptions <| UpdateAlign align)
    in
    Lib.Views.toggleButtons (not options.rowView)
        []
        [ { active = options.align == Left
          , onPress = onPress Left
          , label = Util.viewIcon [ Element.height (Element.px 20) ] FeatherIcons.alignLeft
          }
        , { active = options.align == Center
          , onPress = onPress Center
          , label = Util.viewIcon [ Element.height (Element.px 20) ] FeatherIcons.alignCenter
          }
        , { active = options.align == Right
          , onPress = onPress Right
          , label = Util.viewIcon [ Element.height (Element.px 20) ] FeatherIcons.alignRight
          }
        ]


finalVMState : Model -> Element Msg
finalVMState model =
    if atLastState model then
        case model.latestVM of
            Ok (Ok value) ->
                Element.paragraph
                    []
                    [ Lib.Views.bold "Finished successfully: "
                    , Element.text <| VM.valueToString value
                    ]

            Ok (Err e) ->
                Element.column
                    []
                    [ Lib.Views.bold "Finished with an error: "
                    , Element.html <| Error.view e
                    ]

            Err _ ->
                Lib.Views.bold "There's more to render! But no renders yet"

    else
        Element.el [ Element.height (Element.px 20) ] Element.none



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        fetchSub =
            case model.fetchStatus of
                Loading _ ->
                    Ports.fetchPageResponse gotPage

                _ ->
                    Sub.none
    in
    Sub.batch
        [ fetchSub
        , Sub.map KeyMsg Keyboard.subscriptions
        , Ports.blurs (\_ -> Blur)
        ]



-- received a new page from JSON
-- note this page is a list of VMs, each representing the start of a chunk


gotPage : ( Int, Value ) -> Msg
gotPage ( pageNum, val ) =
    case Decode.decodeValue (Decode.list VM.decoder) val of
        Ok (vm :: vms) ->
            GotPage <| Ok ( pageNum, Zipper.fromCons vm vms )

        Ok _ ->
            GotPage <| Err "Empty VM list!"

        Err e ->
            GotPage <| Err (Decode.errorToString e)


atLastState : Model -> Bool
atLastState model =
    Zipper.isLast model.chunk && Zipper.isLast model.page && Zipper.isLast model.pages && model.fetchStatus == Idle


atFirstState : Model -> Bool
atFirstState model =
    Zipper.isFirst model.chunk && Zipper.isFirst model.page && Zipper.isFirst model.pages && model.fetchStatus == Idle
