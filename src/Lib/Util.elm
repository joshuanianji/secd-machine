module Lib.Util exposing (..)


wrapAdd : a -> a -> List a -> List a
wrapAdd first last l =
    first :: l ++ [ last ]
