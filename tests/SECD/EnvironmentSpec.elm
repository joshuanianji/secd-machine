module SECD.EnvironmentSpec exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import Lib.Cons as Cons
import SECD.EnvItem as Env
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Environment Stack"
        [ testDecode ]



---- DECODE ----


testDecode : Test
testDecode =
    Test.describe "Decoder/Encoder" <|
        List.map
            (\( title, env ) ->
                Test.test title <|
                    \_ ->
                        Expect.equal (Decode.decodeValue (Env.decoder Decode.int) (Env.encode Encode.int env)) (Ok env)
            )
            [ ( "dummy", Env.Dummy )
            , ( "Empty env item", Env.ListItem [] )
            , ( "EnvItem with elements", Env.ListItem <| List.map Cons.Val [ 5, 6, 3, 4 ] )
            ]
