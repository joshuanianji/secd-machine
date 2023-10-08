module Backend.GetExamplesTask exposing (..)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import BackendTask.Glob as Glob
import BackendTask.File as File
import List.Extra
import Json.Decode



-- parse all files in examples/

type alias ExampleGroup =
    { groupName : String 
    , examples : List Example
    }


type alias Example =
    { -- used as a unique identifier
     fileName : String
     -- full code of the example
    , code : String
     -- pretty name of the example, written in frontmatter
    , name : String
    }


type alias ExampleGroupRaw =
    { path : String 
    , groupOrder : Int 
    , groupName : String 
    , fileName : String 
    }

-- For groupings, I start off the folder naming with a number so I have control over the order
-- I remove the numbers when I parse through the files.
examples : BackendTask FatalError (List ExampleGroup)
examples =
    Glob.succeed ExampleGroupRaw
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "examples/")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal "/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".lisp")
        |> Glob.toBackendTask
        |> BackendTask.andThen transformExampleGroupRaw


transformExampleGroupRaw : List ExampleGroupRaw -> BackendTask FatalError (List ExampleGroup)
transformExampleGroupRaw raws =
    List.Extra.groupWhile (\a b -> a.groupName == b.groupName) raws
        |> List.map (\(raw, rest) ->
            let
                readFiles : BackendTask FatalError (List Example)
                readFiles = raw :: rest 
                    |> List.map (\r -> exampleFile r.path r.fileName )
                    |> BackendTask.combine
            in
            BackendTask.succeed (ExampleGroup raw.groupName)
                |> BackendTask.andMap readFiles
        )
        |> BackendTask.combine



exampleFile : String -> String -> BackendTask FatalError Example 
exampleFile path fileName =
    File.bodyWithFrontmatter (exampleFileDecoder fileName) path 
        |> BackendTask.allowFatal 


exampleFileDecoder : String -> String -> Json.Decode.Decoder Example
exampleFileDecoder fileName body =
    Json.Decode.map (Example fileName body)
        (Json.Decode.field "title" Json.Decode.string)
