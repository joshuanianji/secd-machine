module Views.CompiledSpec exposing (suite)

import Expect exposing (Expectation)
import Test exposing (Test)
import Views.Compiled exposing (transpile)


suite : Test
suite =
    Test.describe "compiledSpec"
        [ testTranspiler ]


testTranspiler : Test
testTranspiler =
    Test.describe "transpiler"
        [ Test.todo "write tests" ]
