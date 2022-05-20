port module Ports exposing (..)

import Json.Encode exposing (Value)



-- updated editor from JS side (e.g. user updates value)


port updatedEditor : (String -> msg) -> Sub msg



-- update code from elm side (e.g. we re-initialize the editor with another code value)


port updateCode : String -> Cmd msg


port initialized : String -> Cmd msg



-- sending VM state between elm and JS
-- each VM state is identified by a unique id


port sendState : ( Int, Value ) -> Cmd msg


port fetchState : Int -> Cmd msg


port fetchStateResponse : (( Int, Value ) -> msg) -> Sub msg
