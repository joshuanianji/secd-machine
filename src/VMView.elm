module VMView exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Json.Encode as Encode
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Lib.Views
import List.Zipper as Zipper exposing (Zipper)
import Ports
import SECD.Error as Error
import SECD.Program exposing (Program)
import SECD.VM as VM exposing (VM, VMResult)



---- MODEL ----


type alias Model =
    { -- current chunk holds the VM states in the current chunk
      chunk : Zipper VM

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

    -- keeping compiled code to view
    , compiled : Program
    }


type alias PagesInfo =
    { maxPages : Int, pageSize : Int, chunkSize : Int }


init : PagesInfo -> Program -> ( Model, Cmd Msg )
init pagesInfo prog =
    let
        vm =
            VM.initRaw prog

        pagesData =
            getPages pagesInfo vm
    in
    ( { chunk = pagesData.initialChunk
      , page = pagesData.initialPage
      , pages = pagesData.pages
      , latestVM = pagesData.result
      , totalStates = pagesData.totalVMs
      , pressedKeys = []
      , pagesInfo = pagesInfo
      , compiled = prog
      }
    , Ports.sendPages pagesData.toJSValues
    )


{-| getPages

given a VM, calculate as many states it can reach
Limit the amount of pages (groups of "chunks" that are stored in SessionStorage similar to a memory page) by maxPages
Limit the amount of chunkVMs a page can store by pageSize
Limit the amount of stets a chunk can store by chunkSize

If we the VM terminates in time, also return the value
Else, return the last unfinished VM state

-}
type alias PagesData =
    { result : Result VM VMResult
    , initialPage : Zipper VM
    , initialChunk : Zipper VM
    , pages : Zipper Int
    , totalVMs : Int
    , toJSValues : List ( Int, Encode.Value )
    }


getPages :
    { maxPages : Int
    , pageSize : Int
    , chunkSize : Int
    }
    -> VM
    -> PagesData
getPages { maxPages, pageSize, chunkSize } vm =
    let
        -- returns the same result as getPages
        getFirstChunk : PagesData
        getFirstChunk =
            let
                ( initialChunk, firstChunkRes ) =
                    VM.evalN vm chunkSize
            in
            { result = firstChunkRes
            , initialPage = Zipper.singleton vm
            , initialChunk = Zipper.fromCons vm initialChunk
            , pages = Zipper.singleton 0
            , totalVMs = List.length initialChunk
            , toJSValues = []
            }

        -- takes in, and returns the same result as getPages
        getFirstPage : PagesData -> PagesData
        getFirstPage pagesData =
            case pagesData.result of
                Ok _ ->
                    pagesData

                Err nextState ->
                    -- program needs more than one chunk
                    -- calculate first page
                    let
                        firstPage =
                            VM.evalPage pageSize chunkSize nextState
                    in
                    { pagesData
                        | result = firstPage.result
                        , initialPage = Zipper.fromCons vm firstPage.chunkVMs
                        , totalVMs = pagesData.totalVMs + firstPage.totalVMCount
                    }

        getRemainingPages : PagesData -> PagesData
        getRemainingPages pagesData =
            case pagesData.result of
                Ok _ ->
                    pagesData

                Err secondPageVM ->
                    -- VM does not terminate in one page. Calculate all pages (until max)
                    let
                        -- builds up all the pages to send to JS
                        helper :
                            Int
                            -> VM
                            -> Int
                            -> List ( Int, Encode.Value )
                            ->
                                { result : Result VM VMResult
                                , pageCount : Int
                                , totalVMCount : Int
                                , cmds : List ( Int, Encode.Value )
                                }
                        helper pageCount vm_ vmCountAcc cmdAcc =
                            if pageCount == maxPages then
                                { result = Err vm_
                                , pageCount = pageCount
                                , totalVMCount = vmCountAcc
                                , cmds = cmdAcc
                                }

                            else
                                let
                                    nthPage =
                                        VM.evalPage pageSize chunkSize vm_
                                in
                                case nthPage.result of
                                    Ok vmResult ->
                                        -- VM terminates in this page
                                        { result = Ok vmResult
                                        , pageCount = pageCount
                                        , totalVMCount = vmCountAcc + nthPage.totalVMCount
                                        , cmds = ( pageCount, Encode.list VM.encode nthPage.chunkVMs ) :: cmdAcc
                                        }

                                    Err newVM ->
                                        helper (pageCount + 1)
                                            newVM
                                            (vmCountAcc + nthPage.totalVMCount)
                                            (( pageCount, Encode.list VM.encode nthPage.chunkVMs ) :: cmdAcc)

                        -- we also send the first page states to JS
                        helperData =
                            helper 1 secondPageVM 0 [ ( 0, Encode.list VM.encode <| Zipper.toList pagesData.initialPage ) ]
                    in
                    { pagesData
                        | result = helperData.result
                        , pages = Zipper.fromCons 0 (List.range 1 helperData.pageCount)
                        , totalVMs = pagesData.totalVMs + helperData.totalVMCount
                        , toJSValues = helperData.cmds
                    }
    in
    getFirstChunk
        |> getFirstPage
        |> getRemainingPages



---- MSG ----


type Msg
    = First
    | Previous
    | Step
    | Last
    | KeyMsg Keyboard.Msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- goes to first state in current chunk
        -- TODO: retrieve first page as well
        First ->
            ( { model | chunk = Zipper.first model.chunk }, Cmd.none )

        Previous ->
            case Zipper.previous model.chunk of
                Just newState ->
                    ( { model | chunk = newState }, Cmd.none )

                Nothing ->
                    -- get previous chunk
                    case Zipper.previous model.page of
                        Just newPage ->
                            let
                                startVM =
                                    Zipper.current newPage

                                ( newChunk, _ ) =
                                    VM.evalN startVM model.pagesInfo.chunkSize

                                _ =
                                    Debug.log "Previous chunk" ()
                            in
                            ( { model
                                | chunk = Zipper.last <| Zipper.fromCons startVM newChunk
                                , page = newPage
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "Need to get next page!"
                            in
                            -- fetch previous page from JS
                            ( model, Cmd.none )

        Step ->
            case Zipper.next model.chunk of
                Just newState ->
                    ( { model | chunk = newState }, Cmd.none )

                Nothing ->
                    -- get next chunk
                    case Zipper.next model.page of
                        Just newPage ->
                            let
                                startVM =
                                    Zipper.current newPage

                                ( newChunk, _ ) =
                                    VM.evalN startVM model.pagesInfo.chunkSize

                                _ =
                                    Debug.log "Next chunk" ()
                            in
                            ( { model
                                | chunk = Zipper.fromCons startVM newChunk
                                , page = newPage
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "Need to get next page!"
                            in
                            -- fetch next page from JS
                            ( model, Cmd.none )

        Last ->
            ( { model | chunk = Zipper.last model.chunk }, Cmd.none )

        KeyMsg keyMsg ->
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
                ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 8
        , Element.spacingXY 8 12
        ]
        [ Element.paragraph
            []
            [ Element.text "Total size: "
            , Lib.Views.bold <| String.fromInt model.totalStates
            ]
        , case model.latestVM of
            Err _ ->
                Element.text "Warning: Latest VM does not terminate!"

            Ok _ ->
                Element.none
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Lib.Views.button First <| Element.text "First"
            , Lib.Views.button Previous <| Element.text "Previous"
            , Lib.Views.button Step <| Element.text "Step"
            , Lib.Views.button Last <| Element.text "Last"
            ]
        , if atLastState model then
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
            Element.none
        , Element.html <| VM.view 6 <| Zipper.current model.chunk
        ]



-- returns true if we're at the last state


atLastState : Model -> Bool
atLastState model =
    Zipper.isLast model.chunk && Zipper.isLast model.page && Zipper.isLast model.pages



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyMsg Keyboard.subscriptions
