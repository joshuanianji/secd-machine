module Lib.UtilSpec exposing (suite)

import Expect
import Fuzz
import Lib.Util exposing (..)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Lib.Util"
        [ getLocationInfoTest ]


getLocationInfoTest : Test
getLocationInfoTest =
    let
        createPageInfo : Int -> Int -> { pageSize : Int, chunkSize : Int }
        createPageInfo pageSize chunkSize =
            { pageSize = pageSize, chunkSize = chunkSize }

        createLocationInfoReturn : Int -> Int -> Int -> { pageNum : Int, pageLocation : Int, chunkLocation : Int }
        createLocationInfoReturn pageNum pageLocation chunkLocation =
            { pageNum = pageNum, pageLocation = pageLocation, chunkLocation = chunkLocation }
    in
    Test.describe "Lib.Util.getLocationInfo"
        [ Test.test "0 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 0 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 0 0)
        , Test.test "1 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 1 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 0 1)
        , Test.test "2 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 2 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 0 2)
        , Test.test "15 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 15 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 1 0)
        , Test.test "14 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 14 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 0 14)
        , Test.test "16 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 16 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 1 1)
        , Test.test "149 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 149 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 9 14)
        , Test.test "150 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 150 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 1 0 0)
        , Test.test "151 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 151 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 1 0 1)
        ]
