module Lib.Cons exposing (..)


type Cons a
    = Nil -- also acts as the Nil pointer in the SECD VM
    | Val a
    | Cons (Cons a) (Cons a) -- can be deeply nested cons



-- constructors


nil : Cons a
nil =
    Nil


single : a -> Cons a
single =
    Val


cons : Cons a -> Cons a -> Cons a
cons =
    Cons


fromList : List a -> Cons a
fromList l =
    case l of
        [] ->
            Nil

        x :: xs ->
            Cons (Val x) (fromList xs)



-- deconstructors
-- can only return a list if the Cons ends in a NIL
-- and if the list is not deeply nested ()


toList : Cons a -> Maybe (List a)
toList c =
    case c of
        Nil ->
            Just []

        Cons (Val a) xs ->
            Maybe.map ((::) a) (toList xs)

        _ ->
            Nothing



-- usually for testing


fromConsList : List (Cons a) -> Cons a
fromConsList l =
    case l of
        [] ->
            Nil

        x :: xs ->
            Cons x (fromConsList xs)



-- displays the cons array similar to how Lisp does it


toString : Cons a -> (a -> String) -> String
toString c aToString =
    case c of
        Nil ->
            "Nil"

        Val a ->
            aToString a

        ct ->
            "(" ++ toStringHelper ct aToString ++ ")"


toStringHelper : Cons a -> (a -> String) -> String
toStringHelper c aToString =
    case c of
        Nil ->
            "Nil"

        Val a ->
            aToString a

        -- end of list
        Cons a Nil ->
            toString a aToString

        -- add a dot
        Cons ca (Val a) ->
            toString ca aToString ++ " . " ++ aToString a

        -- run toStringHelper on the remaining, because we don't need parentheses
        Cons ca rest ->
            toString ca aToString ++ " " ++ toStringHelper rest aToString
