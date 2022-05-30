module Views.Compiled exposing
    ( APType(..)
    , Code(..)
    , Indexed(..)
    , Model
    , Msg
    , Unindexed(..)
    , getIndices
    , init
    , stripIndices
    , subscriptions
    , transpile
    , update
    , view
    )

-- | Compiled View
-- Views the compiled code.

import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Lib.Colours as Colours
import Lib.Util as Util
import Lib.Views
import List.Zipper as Zipper exposing (Zipper)
import Ordinal exposing (ordinal)
import Ports
import SECD.Error as Error exposing (Error)
import SECD.Program as Prog exposing (Program)
import SECD.VM as VM exposing (VM, VMResult)
import Set exposing (Set)



---- MODEL ----


type alias Model =
    { -- remove original after more testing
      original : List Prog.Op
    , transpiled : Result Error OkModel
    }


type alias OkModel =
    { code : List Indexed
    , hovered : Maybe Int
    , selected : Set Int
    }


init : Program -> ( Model, Cmd Msg )
init prog =
    let
        transpiled =
            transpile <| Prog.toList prog

        transpiledModel =
            Result.map (\indexed -> { code = indexed, hovered = Nothing, selected = Set.empty }) transpiled
    in
    ( { original = Prog.toList prog
      , transpiled = transpiledModel
      }
    , Cmd.none
    )



-- convert the Program code to our internal representation, useful for UI stuff


transpile : List Prog.Op -> Result Error (List Indexed)
transpile =
    transpile_ 0 >> Result.map Tuple.second



-- transpile helper function alsp keeps track of a "counter" to uniquely identify each code node


transpile_ : Int -> List Prog.Op -> Result Error ( Int, List Indexed )
transpile_ n ops =
    case ops of
        Prog.NIL :: xs ->
            initSingle n NIL
                |> prependTranspiled xs

        (Prog.LD coords) :: xs ->
            initSingle n (LD coords)
                |> prependTranspiled xs

        (Prog.LDC x) :: xs ->
            initSingle n (LDC x)
                |> prependTranspiled xs

        Prog.LDF :: (Prog.FUNCBODY name body) :: xs ->
            initChain n (\funcBody rest -> Indexed ( n, LDFunc name funcBody ) :: rest)
                |> chainTranspiled body
                |> chainTranspiled xs

        Prog.LDF :: (Prog.NESTED nested) :: Prog.AP :: xs ->
            initChain n (\funcBody rest -> Indexed ( n, LDApply AP funcBody ) :: rest)
                |> chainTranspiled nested
                |> chainTranspiled xs

        Prog.LDF :: (Prog.NESTED nested) :: Prog.RAP :: xs ->
            initChain n (\funcBody rest -> Indexed ( n, LDApply RAP funcBody ) :: rest)
                |> chainTranspiled nested
                |> chainTranspiled xs

        Prog.LDF :: (Prog.NESTED nested) :: xs ->
            initChain n (\funcBody rest -> Indexed ( n, LDLambda funcBody ) :: rest)
                |> chainTranspiled nested
                |> chainTranspiled xs

        Prog.RTN :: xs ->
            initSingle n RTN
                |> prependTranspiled xs

        Prog.SEL :: (Prog.NESTED nestedT) :: (Prog.NESTED nestedF) :: xs ->
            initChain n (\onT onF rest -> Indexed ( n, SEL onT onF ) :: rest)
                |> chainTranspiled nestedT
                |> chainTranspiled nestedF
                |> chainTranspiled xs

        Prog.JOIN :: xs ->
            initSingle n JOIN
                |> prependTranspiled xs

        Prog.DUM :: xs ->
            initSingle n DUM
                |> prependTranspiled xs

        (Prog.FUNC f) :: xs ->
            initSingle n (FUNC <| Prog.funcToString f)
                |> prependTranspiled xs

        Prog.AP :: xs ->
            initSingle n LoneAP
                |> prependTranspiled xs

        [] ->
            Ok ( n, [] )

        op :: _ ->
            Err <| "Unexpected op! " ++ Prog.opToString op



-- when we just want one element
-- Kind of like a monadic succeed


initChain : Int -> a -> Result Error ( Int, a )
initChain n a =
    Ok ( n + 1, a )



-- special case of initChain


initSingle : Int -> Code Indexed -> Result Error ( Int, Indexed )
initSingle n a =
    initChain n (Indexed ( n, a ))



-- add an element to the chain
-- a bit like a monadic bind


chainTranspiled : List Prog.Op -> Result Error ( Int, List Indexed -> b ) -> Result Error ( Int, b )
chainTranspiled ops =
    Result.andThen
        (\( startN, f ) ->
            transpile_ startN ops
                |> Result.map (Tuple.mapSecond <| \rest -> f rest)
        )



-- add a transpiled list to the end of a single indexed code
-- this is a special case of chain


prependTranspiled : List Prog.Op -> Result Error ( Int, Indexed ) -> Result Error ( Int, List Indexed )
prependTranspiled ops =
    Result.andThen
        (\( n, idxed ) ->
            transpile_ n ops
                |> Result.map (Tuple.mapSecond <| \rest -> idxed :: rest)
        )



-- each code block will be indexed by a unique ID


type Indexed
    = Indexed ( Int, Code Indexed )



-- an unindexed code block is just the code itself. Stripping the indices is sometimes useful for testing


type Unindexed
    = Unindexed (Code Unindexed)


stripIndices : Indexed -> Unindexed
stripIndices (Indexed ( _, code )) =
    case code of
        NIL ->
            Unindexed NIL

        LD coords ->
            Unindexed (LD coords)

        LDC x ->
            Unindexed (LDC x)

        LDFunc name body ->
            Unindexed (LDFunc name (List.map stripIndices body))

        LDApply aptype nested ->
            Unindexed (LDApply aptype (List.map stripIndices nested))

        LDLambda nested ->
            Unindexed (LDLambda (List.map stripIndices nested))

        SEL nestedT nestedF ->
            Unindexed (SEL (List.map stripIndices nestedT) (List.map stripIndices nestedF))

        RTN ->
            Unindexed RTN

        JOIN ->
            Unindexed JOIN

        DUM ->
            Unindexed DUM

        FUNC f ->
            Unindexed (FUNC f)

        LoneAP ->
            Unindexed LoneAP



-- other times, we only want the indices


getIndices : List Indexed -> List Int
getIndices ops =
    let
        getIndex : Indexed -> List Int
        getIndex (Indexed ( n, code )) =
            case code of
                LDApply _ nested ->
                    n :: getIndices nested

                LDLambda nested ->
                    n :: getIndices nested

                SEL nestedT nestedF ->
                    n :: getIndices nestedT ++ getIndices nestedF

                LDFunc _ nested ->
                    n :: getIndices nested

                -- single elements
                _ ->
                    [ n ]
    in
    List.map getIndex ops
        |> List.concat



-- gets all function definitions
-- returns list of (Index, (funcName, funcBody))


getFuncDefs : List Indexed -> List ( Int, ( String, List Indexed ) )
getFuncDefs ops =
    let
        getFuncDef : Indexed -> Maybe ( Int, ( String, List Indexed ) )
        getFuncDef (Indexed ( n, code )) =
            case code of
                LDFunc name body ->
                    Just ( n, ( name, body ) )

                _ ->
                    Nothing
    in
    List.filterMap getFuncDef ops



-- I need to beef up the program type (List Op) into something more helpful
-- Thus, I traverse the compiled ops and generate my own tree structure used just for a interactive with the code
-- a lot of these changes can simply be built into the Program type itself, but I want to keep the program type as simple as possible


type Code a
    = NIL
    | LD ( Int, Int )
    | LDC Int
    | LDFunc String (List a) -- Loads a function name (when a function is defined in a let stmt)
    | LDLambda (List a) -- loads a lambda (when a lambda is an argument to a function)
    | LDApply APType (List a) -- loads a function and apply it
    | LoneAP -- if a function is loaded from the env then applied
    | RTN
    | SEL (List a) (List a)
    | JOIN
    | DUM
    | FUNC String -- Builtin function (just use their string representation lol)


type APType
    = AP
    | RAP



---- MSG ----


type Msg
    = ToggleSelected Int
    | Hover Int
    | UnHover Int



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.transpiled, msg ) of
        ( Ok okModel, ToggleSelected n ) ->
            if Set.member n okModel.selected then
                ( { model | transpiled = Ok { okModel | selected = Set.remove n okModel.selected } }, Cmd.none )

            else
                ( { model | transpiled = Ok { okModel | selected = Set.insert n okModel.selected } }, Cmd.none )

        ( Ok okModel, Hover n ) ->
            ( { model | transpiled = Ok { okModel | hovered = Just n } }, Cmd.none )

        ( Ok okModel, UnHover _ ) ->
            ( { model | transpiled = Ok { okModel | hovered = Nothing } }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    case model.transpiled of
        Err e ->
            Element.html <| Error.view e

        Ok okModel ->
            viewOk okModel


viewOk : OkModel -> Element Msg
viewOk model =
    let
        viewCodes code =
            List.map (viewCode model) code
                |> List.intersperse [ Element.text "," ]
                |> List.concat
                |> Element.wrappedRow
                    [ Element.width Element.fill ]
    in
    Element.column
        []
        [ -- possible function definitions
          List.map
            (\( n, ( name, body ) ) ->
                Element.row
                    []
                    [ Element.text name, viewCodes body ]
            )
            (getFuncDefs model.code)
            |> Element.column [ Element.spacing 4 ]

        -- the compiled code
        , viewCodes model.code
        ]


viewCode : OkModel -> Indexed -> List (Element Msg)
viewCode model (Indexed ( n, code )) =
    let
        addIf : Bool -> List a -> List a -> List a
        addIf condition xs ys =
            if condition then
                ys ++ xs

            else
                ys

        baseElem color text =
            Element.el
                [ Events.onClick (ToggleSelected n)
                , Events.onMouseEnter (Hover n)
                , Events.onMouseLeave (UnHover n)
                , Element.pointer
                , Element.paddingXY 3 0
                , Font.color Colours.transparent
                , Font.bold
                , Element.behindContent <|
                    Element.el
                        ([ Element.centerX
                         , Font.regular
                         , Font.color Colours.black
                         ]
                            |> addIf (Set.member n model.selected || model.hovered == Just n) [ Font.color color, Font.bold ]
                            |> addIf (Set.member n model.selected && model.hovered == Just n) [ Font.underline ]
                        )
                        (Element.text text)
                ]
                (Element.text text)

        -- attrs for the "main elmeent" in a code block
        mainElem =
            baseElem Colours.purple

        secondaryElem =
            baseElem Colours.orange

        tertiaryElem =
            baseElem Colours.red

        whocaresElem text =
            Element.el
                [ Events.onClick (ToggleSelected n)
                , Events.onMouseEnter (Hover n)
                , Events.onMouseLeave (UnHover n)
                , Element.pointer
                , Font.color Colours.black
                ]
                (Element.text text)

        apToString : APType -> String
        apToString apType =
            case apType of
                AP ->
                    "AP"

                RAP ->
                    "RAP"

        viewNested : List Indexed -> List (Element Msg)
        viewNested =
            List.map (viewCode model)
                >> List.intersperse [ Element.text "," ]
                >> List.concat
    in
    case code of
        NIL ->
            [ mainElem "NIL" ]

        LD ( x, y ) ->
            [ mainElem <| "LD (" ++ String.fromInt x ++ "." ++ String.fromInt y ++ ")" ]

        LDC x ->
            [ mainElem <| "LDC " ++ String.fromInt x ]

        LDFunc name _ ->
            [ mainElem "LDF"
            , whocaresElem ","
            , secondaryElem name
            ]

        LDApply aptype nested ->
            [ mainElem "LDF"
            , whocaresElem ","
            , secondaryElem "["
            ]
                ++ viewNested nested
                ++ [ secondaryElem "]"
                   , whocaresElem ","
                   , tertiaryElem <| apToString aptype
                   ]

        LDLambda nested ->
            [ mainElem "LDF"
            , whocaresElem ","
            , secondaryElem "["
            ]
                ++ viewNested nested
                ++ [ secondaryElem "]" ]

        SEL nestedT nestedF ->
            [ mainElem "SEL"
            , whocaresElem ","
            , secondaryElem "["
            ]
                ++ viewNested nestedT
                ++ [ secondaryElem "]"
                   , tertiaryElem "["
                   ]
                ++ viewNested nestedF
                ++ [ tertiaryElem "]" ]

        RTN ->
            [ mainElem "RTN" ]

        JOIN ->
            [ mainElem "JOIN" ]

        DUM ->
            [ mainElem "DUM" ]

        FUNC f ->
            [ mainElem f ]

        LoneAP ->
            [ mainElem "AP" ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
