module SECD.VMEnv exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Lib.Cons as Cons exposing (Cons)
import Lib.Util as Util



-- "Environment" stack used by the VM when executing
-- Environment stack is essentially a list of lists, so it's a little more complicated than that
-- putting this in its own file for organization


type alias Environment a =
    List (EnvItem a)


type EnvItem a
    = ListItem (List (Cons a)) -- this ensures the Environment is at least nested 2 deep.
    | Dummy -- with doing recursion



-- debug


view : (a -> Html msg) -> Environment a -> Html msg
view viewA env =
    List.map (viewEnvItem viewA) env
        |> Html.div
            [ Attr.class "vm-body row env-body" ]


viewEnvItem : (a -> Html msg) -> EnvItem a -> Html msg
viewEnvItem viewA item =
    case item of
        ListItem cs ->
            List.map (Cons.view viewA) cs
                |> List.intersperse (Html.text ",")
                |> Util.wrapAdd (Html.text "(") (Html.text ")")
                |> Html.div [ Attr.class "vm-env row complex vm-listitem" ]

        Dummy ->
            Html.div [ Attr.class "vm-env dummy" ] [ Html.text "Dummy" ]


envItemToString : (a -> String) -> EnvItem a -> String
envItemToString aToString item =
    case item of
        ListItem c ->
            "[" ++ (String.join ", " <| List.map (Cons.toString aToString) c) ++ "]"

        Dummy ->
            "Dummy"



-- ops


init : Environment a
init =
    []


fromList : List (List a) -> Environment a
fromList =
    List.map (ListItem << List.map Cons.Val)


push : List (Cons a) -> Environment a -> Environment a
push xs env =
    ListItem xs :: env


pushDummy : Environment a -> Environment a
pushDummy env =
    Dummy :: env



-- locate is 0-indexed


locate : ( Int, Int ) -> Maybe (List (Cons a)) -> Environment a -> Result String (Cons a)
locate ( x, y ) mDummyVal environment =
    if (x < 0) || (y < 0) then
        Err "Negative index out of bounds"

    else
        case ( x, environment ) of
            ( _, [] ) ->
                Err "Locate: out of bounds!"

            ( 0, (ListItem h) :: _ ) ->
                locateRow y h

            ( 0, Dummy :: _ ) ->
                case mDummyVal of
                    Nothing ->
                        Err "Locate: attempt to access uninitialized Dummy Env value!"

                    Just dummyVal ->
                        locateRow y dummyVal

            ( _, _ :: t ) ->
                locate ( x - 1, y ) mDummyVal t


locateRow : Int -> List a -> Result String a
locateRow y row =
    case ( y, row ) of
        ( _, [] ) ->
            Err "Locate: out of bounds!"

        ( 0, h :: _ ) ->
            Ok h

        ( _, _ :: t ) ->
            locateRow (y - 1) t



-- replace the first row, IFF it is a dummy value


replaceDummy : List (Cons a) -> Environment a -> Result String (Environment a)
replaceDummy newRow env =
    case env of
        [] ->
            Err "replaceDummy: empty environment!"

        Dummy :: t ->
            Ok <| ListItem newRow :: t

        _ ->
            Err ""
