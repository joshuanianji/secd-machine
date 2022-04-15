module SECD.VMSpec exposing (..)

import Expect
import SECD.Program as Prog exposing (Op(..))
import SECD.VM as VM
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Test "
        [ Test.test "Returns Nil in a Nil program" <|
            \_ ->
                let
                    program =
                        Prog.fromSingleton NIL
                in
                Expect.equal (VM.evaluate <| VM.init program) (Ok VM.Nil)
        , Test.test "Returns Constant in a LDC program" <|
            \_ ->
                let
                    program =
                        Prog.fromSingleton (LDC 5)
                in
                Expect.equal (VM.evaluate <| VM.init program) (Ok <| VM.Integer 5)
        ]
