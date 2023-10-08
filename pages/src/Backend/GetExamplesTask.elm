module Backend.GetExamplesTask exposing (..)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import BackendTask.Glob as Glob
import List.Extra


-- parse all files in examples/

type alias ExampleGroup =
    { groupName : String 
    , examples : List Example
    }


type alias Example =
    { -- pretty name of the example
      name : String
    , code : String
      -- used as a unique identifier
    , fileName : String
    }

-- For groupings, I start off the folder naming with a number so I have control over the order
-- I remove the numbers when I parse through the files.
exampleGroups : BackendTask FatalError (List String)
exampleGroups =
    Glob.succeed (\slug -> slug)
        |> Glob.match (Glob.literal "examples/")
        |> Glob.match Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal "/")
        |> Glob.match Glob.wildcard
        |> Glob.match (Glob.literal ".lisp")
        |> Glob.toBackendTask
        |> BackendTask.map List.Extra.unique

-- examples : List String -> BackendTask (List ExampleGroup)
-- examples = 
--     let
--         exampleFiles slug =
--             Glob.succeed (\name -> name)
--                 |> Glob.match (Glob.literal "examples/")
--                 |> Glob.match (Glob.literal slug)
--                 |> Glob.match (Glob.literal "/")
--                 |> Glob.capture Glob.wildcard
--                 |> Glob.match (Glob.literal ".lisp")
--                 |> Glob.toBackendTask
--                 |> BackendTask.map (\examples -> { groupName = slug, examples = examples })
-- --     BackendTask.map 

-- example : String -> BackendTask Example
-- example slug = 
