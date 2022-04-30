module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as Attr
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util exposing (eachZero)
import Lib.Views
import Ports
import SECD.Error exposing (Error)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
import Set exposing (Set)
import VMView


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { code : String
    , openTabs : Set String
    , compiled : CompiledState
    }


type CompiledState
    = Idle
    | ParseError Error
    | CompileError AST Error
    | CompileSuccess AST VMView.Model


init : ( Model, Cmd Msg )
init =
    ( { code = ""
      , openTabs = Set.empty
      , compiled = Idle
      }
    , Ports.initialized ""
    )



---- MSG ----


type Msg
    = Remonke
    | ToggleTab String
      -- code changed from JS side
    | CodeChanged String
      -- code changed from Elm side
    | UpdateCode String
    | Compile
    | VMViewMsg VMView.Msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.compiled, msg ) of
        ( _, Remonke ) ->
            ( model, Ports.initialized model.code )

        ( _, ToggleTab tab ) ->
            if Set.member tab model.openTabs then
                ( { model | openTabs = Set.remove tab model.openTabs }, Cmd.none )

            else
                ( { model | openTabs = Set.insert tab model.openTabs }, Cmd.none )

        ( _, CodeChanged newCode ) ->
            ( { model | code = newCode }, Cmd.none )

        ( _, UpdateCode newCode ) ->
            ( model, Ports.updateCode newCode )

        ( _, Compile ) ->
            case AST.parse model.code of
                Err deadends ->
                    ( { model | compiled = ParseError <| Util.deadEndsToString deadends }, Cmd.none )

                Ok ast ->
                    case Prog.compile ast of
                        Err err ->
                            ( { model | compiled = CompileError ast err }, Cmd.none )

                        Ok prog ->
                            ( { model | compiled = CompileSuccess ast (VMView.init prog) }, Cmd.none )

        ( CompileSuccess ast vmModel, VMViewMsg subMsg ) ->
            let
                ( newVMModel, newVMMsg ) =
                    VMView.update subMsg vmModel
            in
            ( { model | compiled = CompileSuccess ast newVMModel }, Cmd.map VMViewMsg newVMMsg )

        ( _, _ ) ->
            ( model, Cmd.none )


type alias Programs =
    List ( String, List ( String, String ) )


programs : Programs
programs =
    [ ( "Basics"
      , [ ( "Arithmetic", "(* (+ 1 2) (+ 3 4))" )
        , ( "Comparison", "(<= 1 2)" )
        , ( "Let Statements", "(let ((x 1)) (+ x 2))" )
        , ( "If Statements", "(if (> 1 2) 1 2)" )
        , ( "Letrec Statements", "(letrec ((x 1)) (+ x 2))" )
        ]
      )
    , ( "Lists"
      , [ ( "Length of a List - Recursion", "(letrec\n \t((f (lambda (x m) (if (null x) m (f (cdr x) (+ m 1))))))\n\t(f '(1 2 3) 0))" )
        , ( "Map list - Recursion", "(letrec \n \t((maplist (lambda (f l) (if (null l) nil (cons (f (car l)) (maplist f (cdr l))))))\n\t (add1 (lambda (x) (+ x 1))))\n\t(maplist add1 '(1 2 3)) \n)" )
        , ( "Sum a list (Recursion and foldr)", "(letrec \n \t ; sums a list directly (using recursion)\n \t((sumlistDirect (lambda (l) (if (null l) 0 (+ (car l) (sumlist (cdr l))))))\n \t (foldr (lambda (f z l) (if (null l) z (foldr f (f z (car l)) (cdr l)))))\n \t ; rewriting add using lambdas - this allows us to use higher order functions\n \t (add (lambda (x y) (+ x y)))\n \t ; sums a list using foldr\n \t (sumlist (lambda (l) (foldr add 0 l))))\n \n \t; update the line from calling `sumlistDirect` to `sumList`!\n \t; note how both function calls produce the same value\n\t(sumlist '(1 2 3)) \n)" )
        ]
      )
    , ( "Complex"
      , [ ( "Mutual Recursion", "(letrec\n\t((odd \t(lambda (n) (if (eq n 0) nil (even (- n 1)))))\n\t (even \t(lambda (n) (if (eq n 0) t\t (odd  (- n 1)))))) \n \t(even 4))" )
        , ( "Manual Currying", "(let\n \t((curriedAdd (lambda (x) (lambda (y) (+ x y)))))\n \t((curriedAdd 5) 10))" )
        ]
      )
    ]



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family
            [ Font.typeface "Avenir"
            , Font.typeface "Helvetica"
            , Font.typeface "Arial"
            , Font.sansSerif
            ]
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 8
            ]
            [ Element.el
                [ Font.size 36
                , Font.bold
                , Element.centerX
                , Element.paddingXY 0 16
                ]
              <|
                Element.text "SECD Machine"
            , Element.el
                [ Font.size 18
                , Font.bold
                , Element.centerX
                , Element.paddingXY 0 8
                ]
              <|
                Element.text "An implementation as seen in Ualberta's CMPUT 325"
            , codeEditor model
            , Element.row
                [ Element.spacing 8
                , Element.centerX
                ]
                [ Lib.Views.button Remonke <| Element.text "Rerun Monkey"
                , Lib.Views.button Compile <| Element.text "Parse + Compile"
                ]
            , case model.compiled of
                Idle ->
                    Element.none

                ParseError err ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.size 24 ] <| Element.text "Parse error!"
                        , Element.paragraph [ Font.size 16 ] [ Element.text err ]
                        ]

                CompileError ast err ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.size 24, Font.bold ] <| Element.text "Parse success"
                        , Element.paragraph [ Font.size 16 ] [ Element.text <| Debug.toString ast ]
                        , Element.el [ Font.size 24, Font.bold ] <| Element.text "Compile error!"
                        , Element.paragraph [ Font.size 16 ] [ Element.text err ]
                        ]

                CompileSuccess ast vmModel ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 8
                        ]
                        [ Element.el [ Font.size 24, Font.bold ] <| Element.text "Parse success"
                        , Element.paragraph [ Font.size 16 ] [ Element.text <| Debug.toString ast ]
                        , Element.el [ Font.size 24, Font.bold ] <| Element.text "Compilation success!"
                        , Element.map VMViewMsg <| VMView.view vmModel
                        ]
            ]


codeEditor : Model -> Element Msg
codeEditor model =
    let
        viewPrograms : List ( String, List ( String, String ) ) -> Element Msg
        viewPrograms progs =
            Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.paddingXY 4 12
                ]
            <|
                List.map
                    (\( name, progTuples ) ->
                        let
                            ( icon, content ) =
                                if Set.member name model.openTabs then
                                    ( FeatherIcons.chevronUp, Element.el [ Element.paddingEach { eachZero | left = 16 } ] <| viewProgramsTab progTuples )

                                else
                                    ( FeatherIcons.chevronDown, Element.none )
                        in
                        Element.column
                            [ Element.width Element.fill ]
                            [ Element.row
                                [ Element.paddingXY 16 12
                                , Events.onClick <| ToggleTab name
                                , Element.pointer
                                , Element.spacing 4
                                ]
                                [ Element.text name
                                , Util.viewIcon [] icon
                                ]
                            , content
                            ]
                    )
                    progs

        viewProgramsTab : List ( String, String ) -> Element Msg
        viewProgramsTab progValues =
            Element.column
                [ Element.width Element.fill ]
            <|
                List.map
                    (\( name, prog ) ->
                        Input.button
                            [ Element.paddingXY 12 8 ]
                            { onPress = Just <| UpdateCode prog
                            , label = Element.text name
                            }
                    )
                    progValues
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.el
            [ Element.width <| Element.fillPortion 5
            , Background.color <| Element.rgb255 38 50 56
            , Element.height Element.fill
            ]
          <|
            Element.html (Html.div [ Attr.id "editor" ] [])
        , Element.el
            [ Element.width <| Element.fillPortion 2
            , Element.height <| Element.px 300
            , Element.scrollbars
            ]
          <|
            viewPrograms programs
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        vmModelSub =
            case model.compiled of
                CompileSuccess _ vmModel ->
                    VMView.subscriptions vmModel

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map VMViewMsg vmModelSub
        , Ports.updatedEditor CodeChanged
        ]
