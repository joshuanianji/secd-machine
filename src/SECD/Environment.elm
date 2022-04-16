module SECD.Environment exposing (..)

-- Environment stack needs to be a list of lists, so it's a little more complicated.alias


type alias Environment a =
    List (List a)



-- ops


init : Environment a
init =
    []



-- locate is 0-indexed


locate : ( Int, Int ) -> Environment a -> Result String a
locate ( x, y ) environment =
    if (x < 0) || (y < 0) then
        Err "Negative index out of bounds"

    else
        case ( x, environment ) of
            ( _, [] ) ->
                Err "Locate: out of bounds!"

            ( 0, h :: _ ) ->
                locateRow y h

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
