port module Ports exposing (..)

-- updated editor from JS side (e.g. user updates value)


port updatedEditor : (String -> msg) -> Sub msg



-- update code from elm side (e.g. we re-initialize the editor with another code value)


port updateCode : String -> Cmd msg


port initialized : String -> Cmd msg
