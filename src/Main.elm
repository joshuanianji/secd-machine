module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Lib.LispAST as AST exposing (AST)
import Lib.Util as Util
import Ports
import SECD.Error exposing (Error)
import SECD.Program as Prog exposing (Cmp(..), Func(..), Op(..))
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
      , compiled = Idle
      }
    , Cmd.none
    )



---- MSG ----


type Msg
    = CodeChanged String
    | Compile
    | VMViewMsg VMView.Msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.compiled, msg ) of
        ( _, CodeChanged newCode ) ->
            ( { model | code = newCode }, Cmd.none )

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



-- recursively calculates the length of a list
{--
(letrec (f (λx m | (if (null x) m (f (cdr x) (+ m 1) )) ) )
(f '(1 2 3) 0) )
--}


recLength : Prog.Program
recLength =
    let
        func =
            [ LD ( 0, 0 ), FUNC NULL, SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ NIL, LDC 1, LD ( 0, 1 ), FUNC ADD, FUNC CONS, LD ( 0, 0 ), FUNC CDR, FUNC CONS, LD ( 1, 0 ), AP, JOIN ], RTN ]

        -- (f '(1 2 3) 0)
        funcApply =
            [ NIL, LDC 0, FUNC CONS, NIL, LDC 3, FUNC CONS, LDC 2, FUNC CONS, LDC 1, FUNC CONS, FUNC CONS, LD ( 0, 0 ), AP, RTN ]
    in
    Prog.fromList
        [ DUM, NIL, LDF, NESTED func, FUNC CONS, LDF, NESTED funcApply, RAP ]


factorial : Prog.Program
factorial =
    let
        fact =
            [ LDC 0, LD ( 0, 0 ), FUNC (COMPARE CMP_EQ), SEL, NESTED [ LD ( 0, 1 ), JOIN ], NESTED [ NIL, LD ( 0, 1 ), LD ( 0, 0 ), FUNC MULT, FUNC CONS, LD ( 2, 1 ), LD ( 0, 0 ), FUNC SUB, FUNC CONS, LD ( 1, 0 ), AP, JOIN ], RTN ]

        -- I actually don't really know what this does
        factCreateClosure =
            [ NIL, LD ( 1, 1 ), FUNC CONS, LD ( 1, 0 ), FUNC CONS, LD ( 0, 0 ), AP, RTN ]
    in
    Prog.fromList [ NIL, LDC 1, FUNC CONS, LDC 6, FUNC CONS, LDF, NESTED [ DUM, NIL, LDF, NESTED fact, FUNC CONS, LDF, NESTED factCreateClosure, RAP, RTN ], AP ]


letLambda : Prog.Program
letLambda =
    Prog.fromList [ NIL, LDF, NESTED [ LDC 1, LD ( 0, 0 ), FUNC ADD, RTN ], FUNC CONS, LDF, NESTED [ NIL, LDC 3, FUNC CONS, LD ( 0, 0 ), AP, RTN ], AP ]



-- I'm not sure how mutually recursive functions work, so let's try it out!


mutuallyRecursiveIsEven : Int -> Prog.Program
mutuallyRecursiveIsEven n =
    let
        isEven =
            mutualRecursive [ NIL ] ( 1, 1 )

        isOdd =
            mutualRecursive [ NIL, FUNC ATOM ] ( 1, 0 )

        mutualRecursive onTrue letrecCoords =
            [ LDC 0, LD ( 0, 0 ), FUNC (COMPARE CMP_EQ), SEL, NESTED <| onTrue ++ [ JOIN ], NESTED [ NIL, LDC 1, LD ( 0, 0 ), FUNC SUB, FUNC CONS, LD letrecCoords, AP, JOIN ], RTN ]

        body =
            [ NIL, LDC n, FUNC CONS, LD ( 0, 1 ), AP, RTN ]
    in
    Prog.fromList
        [ DUM, NIL, LDF, NESTED isOdd, FUNC CONS, LDF, NESTED isEven, FUNC CONS, LDF, NESTED body, RAP ]



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ Html.div [ Attr.class "btns" ]
            [ Html.button [ Events.onClick Compile ] [ Html.text "Parse + Compile" ]
            ]
        , case model.compiled of
            Idle ->
                Html.text ""

            ParseError err ->
                Html.div []
                    [ Html.h3 [] [ Html.text "Parse error!" ]
                    , Html.p [] [ Html.text err ]
                    ]

            CompileError ast err ->
                Html.div []
                    [ Html.h3 [] [ Html.text "Parse success" ]
                    , Html.p [] [ Html.text <| Debug.toString ast ]
                    , Html.h3 [] [ Html.text "Compile error!" ]
                    , Html.p [] [ Html.text err ]
                    ]

            CompileSuccess ast vmModel ->
                Html.div [ Attr.class "fill-width" ]
                    [ Html.h3 [] [ Html.text "Parse success" ]
                    , Html.p [] [ Html.text <| Debug.toString ast ]
                    , Html.map VMViewMsg <| VMView.view vmModel
                    ]
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
