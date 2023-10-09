module Backend.GetExamplesTask exposing (..)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import BackendTask.Glob as Glob
import BackendTask.File as File
import List.Extra
import Json.Decode
import List.Nonempty as Nonempty exposing (Nonempty)
import FatalError


-- parse all files in examples/

type alias ExampleGroup =
    { groupName : String 
    , examples : Nonempty Example
    }

-- Get default example (first example in first group)
getDefault : Nonempty ExampleGroup -> (String, Example)
getDefault groups =
    let
        firstGroup = Nonempty.head groups
        firstExample = Nonempty.head firstGroup.examples
    in
    (firstGroup.groupName, firstExample)

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
examples : BackendTask FatalError (Nonempty ExampleGroup)
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


transformExampleGroupRaw : List ExampleGroupRaw -> BackendTask FatalError (Nonempty ExampleGroup)
transformExampleGroupRaw raws =
    List.Extra.groupWhile (\a b -> a.groupName == b.groupName) raws
        |> List.map (\(raw, rest) ->
            let
                readFiles : BackendTask FatalError (Nonempty Example)
                readFiles = raw :: rest 
                    |> List.map (\r -> exampleFile r.path r.fileName )
                    |> BackendTask.combine
                    |> BackendTask.andThen (\l -> 
                        case Nonempty.fromList l of 
                            Nothing -> BackendTask.fail (FatalError.fromString <| "No examples found in group " ++ raw.groupName)
                            Just nonempty -> BackendTask.succeed nonempty
                    )
            in
            BackendTask.succeed (ExampleGroup raw.groupName)
                |> BackendTask.andMap readFiles
        )
        |> BackendTask.combine
        |> BackendTask.andThen (\l ->
            case Nonempty.fromList l of 
                Nothing -> BackendTask.fail (FatalError.fromString "No examples found")
                Just nonempty -> BackendTask.succeed nonempty
            )



exampleFile : String -> String -> BackendTask FatalError Example 
exampleFile path fileName =
    File.bodyWithFrontmatter (exampleFileDecoder fileName) path 
        |> BackendTask.allowFatal 


exampleFileDecoder : String -> String -> Json.Decode.Decoder Example
exampleFileDecoder fileName body =
    Json.Decode.map (Example fileName body)
        (Json.Decode.field "title" Json.Decode.string)
