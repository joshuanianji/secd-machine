port module Ports exposing (..)

import Json.Encode exposing (Value)



-- updated editor from JS side (e.g. user updates value)


port updatedEditor : (String -> msg) -> Sub msg



-- update code from elm side (e.g. we re-initialize the editor with another code value)


port updateCode : String -> Cmd msg


port initialize : String -> Cmd msg



-- sending VM page (a chunk of 250 states) between elm and JS
-- each VM page is identified by a unique id


port sendPages : List ( Int, Value ) -> Cmd msg


port fetchPage : Int -> Cmd msg


port fetchPageResponse : (( Int, Value ) -> msg) -> Sub msg



-- misc


port log : String -> Cmd msg


port blurs : (() -> msg) -> Sub msg
