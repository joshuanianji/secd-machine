module Backend.GetEnv exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Env
import FatalError exposing (FatalError)



-- | get the value of necessary environment variables (for dev and build time)
-- | for now it is ENV


type alias Env =
    { mode : EnvMode
    }


type EnvMode
    = Dev
    | Prod


retrieve : BackendTask FatalError Env
retrieve =
    BackendTask.map Env
        (BackendTask.Env.expect "ENV"
            |> BackendTask.allowFatal
            |> BackendTask.andThen toEnvMode
        )


toEnvMode : String -> BackendTask FatalError EnvMode
toEnvMode e =
    case e of
        "dev" ->
            BackendTask.succeed Dev

        "prod" ->
            BackendTask.succeed Prod

        val ->
            BackendTask.fail (FatalError.fromString ("Invalid ENV value: " ++ val))
