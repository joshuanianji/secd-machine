module SECD.Environment exposing (..)

-- Environment stack needs to be a list of lists, so it's a little more complicated.alias


type alias Environment a =
    List (EnvItem a)


type EnvItem a
    = ListItem (List a)
    | Dummy -- with doing recursion



-- ops


init : Environment a
init =
    []


fromList : List (List a) -> Environment a
fromList =
    List.map ListItem


push : List a -> Environment a -> Environment a
push xs env =
    ListItem xs :: env



-- locate is 0-indexed


locate : ( Int, Int ) -> Environment a -> Result String a
locate ( x, y ) environment =
    if (x < 0) || (y < 0) then
        Err "Negative index out of bounds"

    else
        case ( x, environment ) of
            ( _, [] ) ->
                Err "Locate: out of bounds!"

            ( 0, (ListItem h) :: _ ) ->
                locateRow y h

            ( 0, Dummy :: _ ) ->
                Err "Locate: attempt to access Dummy Env value!"

            ( _, _ :: t ) ->
                locate ( x - 1, y ) t


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


replaceDummy : List a -> Environment a -> Result String (Environment a)
replaceDummy newRow env =
    case env of
        [] ->
            Err "replaceDummy: empty environment!"

        Dummy :: t ->
            Ok <| ListItem newRow :: t

        _ ->
            Err ""
