port module Ports exposing (updatedEditor)


port updatedEditor : (String -> msg) -> Sub msg
